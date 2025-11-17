(* ::Package:: *)

BeginPackage["WolframMediaWiki`", {"WolframMediaWiki`WikitextConverter`"}]

(* Public API *)
ConnectToWiki::usage = "ConnectToWiki[configFile] loads credentials from a config file and establishes a connection to a MediaWiki instance. Returns an Association with connection properties. The config file should return an Association with keys: \"URL\" (base wiki URL without /api.php), \"Username\", \"Password\"."

$CurrentWikiConnection::usage = "$CurrentWikiConnection is the currently active MediaWiki connection Association."

$Debug::usage = "$Debug controls debug output. Set to True to enable detailed logging, False to disable (default: False)."

PublishNotebook::usage = "PublishNotebook[notebook, pageName] converts a notebook to wikitext, uploads all images, and publishes the page to the connected wiki."

PublishWikitext::usage = "PublishWikitext[wikitext, pageName] publishes wikitext content to the connected wiki, automatically uploading any referenced images."

UploadImage::usage = "UploadImage[imageFile, wikiFileName] uploads an image file to the connected wiki with the specified filename."

UploadPage::usage = "UploadPage[pageName, newContent, overwritePage] uploads new content to the specified wiki page."

GetPage::usage = "GetPage[pageName] retrieves the current content of a wiki page."

GetAllPages::usage = "GetAllPages[] retrieves a list of all page titles in the wiki. Use GetAllPages[limit] to limit the number of results."

GetPageLinks::usage = "GetPageLinks[pageName] retrieves all internal wiki links from a page. Use GetPageLinks[pageName, limit] to limit the number of results."

GetImagesForUpload::usage = "GetImagesForUpload[wikitextFile] reads a wikitext file and its img/ subdirectory, returning an Association mapping wiki filenames to local file paths for manual upload."

PrepareWikitextForUpload::usage = "PrepareWikitextForUpload[wikitextContent, maxWidth] replaces [[File:img/filename]] with [[File:nb-filename]] and adds width constraint. Default maxWidth is 800px."

EditPage::usage = "EditPage[pageName, content, summary] edits or creates a wiki page with the given content."

Begin["`Private`"]

$CurrentWikiConnection = None;

(* ========== Authentication ========== *)

ConnectToWiki[configFile_String] := Module[
    {config, url, username, password},

    If[!FileExistsQ[configFile],
        Message[ConnectToWiki::notfound, configFile];
        Return[$Failed]
    ];

    config = Get[configFile];

    If[!AssociationQ[config] ||
       !KeyExistsQ[config, "URL"] ||
       !KeyExistsQ[config, "Username"] ||
       !KeyExistsQ[config, "Password"],
        Message[ConnectToWiki::invalid];
        Return[$Failed]
    ];

    url = config["URL"];
    username = config["Username"];
    password = config["Password"];
    Clear[config];
    connectToWikiInternal[url, username, password]
]

ConnectToWiki::notfound = "Config file not found: `1`";
ConnectToWiki::invalid = "Config file must be an Association with keys: \"URL\", \"Username\", \"Password\"";


(* Internal connection function *)
connectToWikiInternal[url_String, username_String, password_String] := Module[
    {apiEndpoint, loginToken, loginResult, editToken, connection},

    (* Construct API endpoint *)
    apiEndpoint = If[StringEndsQ[url, "/"],
        url <> "api.php",
        url <> "/api.php"
    ];


    loginToken = getLoginToken[apiEndpoint];
    If[loginToken === $Failed,
        Message[ConnectToWiki::loginfailed, "Could not retrieve login token"];
        Return[$Failed]
    ];

    loginResult = performLogin[apiEndpoint, username, password, loginToken];
    If[loginResult === $Failed,
        Message[ConnectToWiki::loginfailed, "Login failed"];
        Return[$Failed]
    ];

    editToken = getEditToken[apiEndpoint, loginResult["Cookies"]];
    If[editToken === $Failed,
        Message[ConnectToWiki::tokenfailed];
        Return[$Failed]
    ];

    (* Create connection association *)
    connection = <|
        "URL" -> url,
        "APIEndpoint" -> apiEndpoint,
        "Username" -> username,
        "Token" -> editToken,
        "Cookies" -> loginResult["Cookies"]
    |>;

    $CurrentWikiConnection = connection;
    connection
]

ConnectToWiki::loginfailed = "Login failed: `1`";
ConnectToWiki::tokenfailed = "Failed to obtain edit token";


getLoginToken[apiEndpoint_String] := Module[
    {response, json},

    Echo[apiEndpoint, "Requesting login token from:"];

    response = URLRead[
        HTTPRequest[apiEndpoint,
            <|"Method" -> "GET",
              "Query" -> {
                  "action" -> "query",
                  "meta" -> "tokens",
                  "type" -> "login",
                  "format" -> "json"
              }
            |>
        ]
    ];

    Echo[response["StatusCode"], "Response status code:"];

    If[response["StatusCode"] != 200,
        Echo[response["Body"], "Response body:"];
        Return[$Failed]
    ];

    json = ImportString[response["Body"], "RawJSON"];
    (* Echo[json, "Response JSON:"]; *)

    Lookup[Lookup[Lookup[json, "query", <||>], "tokens", <||>], "logintoken", $Failed]
]

performLogin[apiEndpoint_String, username_String, password_String, loginToken_String] := Module[
    {response, json, cookies, loginResult},

    Echo["Attempting login..."];

    response = URLRead[
        HTTPRequest[apiEndpoint,
            <|"Method" -> "POST",
              "Body" -> {
                  "action" -> "login",
                  "lgname" -> username,
                  "lgpassword" -> password,
                  "lgtoken" -> loginToken,
                  "format" -> "json"
              }
            |>
        ]
    ];

    Echo[response["StatusCode"], "Login response status:"];

    If[response["StatusCode"] != 200,
        Echo[response["Body"], "Login error body:"];
        Return[$Failed]
    ];

    json = ImportString[response["Body"], "RawJSON"];
    (* Echo[json, "Login response JSON:"]; *)

    loginResult = Lookup[Lookup[json, "login", <||>], "result", ""];
    Echo[loginResult, "Login result:"];

    If[loginResult != "Success",
        Return[$Failed]
    ];

    (* Extract cookies from HTTPResponse object using Part syntax *)
    cookies = response["Cookies"];
    Echo[Length[cookies], "Number of cookies:"];

    <|"Success" -> True, "Cookies" -> cookies|>
]

(* need extra csrf token to get edit permissions *)
getEditToken[apiEndpoint_String, cookies_List] := Module[
    {response, json},

    response = URLRead[
        HTTPRequest[apiEndpoint,
            <|"Method" -> "GET",
              "Query" -> {
                  "action" -> "query",
                  "meta" -> "tokens",
                  "format" -> "json"
              },
              "Cookies" -> cookies
            |>
        ]
    ];

    If[response["StatusCode"] != 200, Return[$Failed]];

    json = ImportString[response["Body"], "RawJSON"];
    Lookup[Lookup[Lookup[json, "query", <||>], "tokens", <||>], "csrftoken", $Failed]
]

GetPage[pageName_String] := Module[
    {response, json, content, pages, pageData, revisions},

    If[$CurrentWikiConnection === None,
        Message[GetPage::noconnection];
        Return[$Failed]
    ];

    Echo[pageName, "Fetching page:"];

    response = URLRead[
        HTTPRequest[$CurrentWikiConnection["APIEndpoint"],
            <|"Method" -> "GET",
              "Query" -> {
                  "action" -> "query",
                  "titles" -> pageName,
                  "prop" -> "revisions",
                  "rvprop" -> "content",
                  "format" -> "json"
              },
              "Cookies" -> $CurrentWikiConnection["Cookies"]
            |>
        ]
    ];

    Echo[response["StatusCode"], "Response status:"];

    If[response["StatusCode"] != 200,
        Echo[response["Body"], "Error body:"];
        Return[$Failed]
    ];

    json = ImportString[response["Body"], "RawJSON"];
    (* Echo[json, "Response JSON:"]; *)

    (* Extract pages from nested structure *)
    pages = Lookup[Lookup[json, "query", <||>], "pages", <||>];
    (* Echo[pages, "Pages:"]; *)

    If[Length[pages] == 0, Return[$Failed]];

    pageData = First[Values[pages]];
    (* Echo[pageData, "Page data:"]; *)

    revisions = Lookup[pageData, "revisions", {}];
    (* Echo[revisions, "Revisions:"]; *)

    If[Length[revisions] == 0, Return[$Failed]];

    (* Extract content from first revision *)
    Lookup[First[revisions], "*", $Failed]
]

GetPage::noconnection = "No active wiki connection. Use ConnectToWiki first.";


(* ========== Get All Pages ========== *)

GetAllPages[limit_Integer: 500] := Module[
    {response, json, allpages},

    If[$CurrentWikiConnection === None,
        Message[GetAllPages::noconnection];
        Return[$Failed]
    ];

    debugEcho[limit, "Fetching pages with limit:"];

    response = URLRead[
        HTTPRequest[$CurrentWikiConnection["APIEndpoint"],
            <|"Method" -> "GET",
              "Query" -> {
                  "action" -> "query",
                  "list" -> "allpages",
                  "aplimit" -> ToString[limit],
                  "format" -> "json"
              },
              "Cookies" -> $CurrentWikiConnection["Cookies"]
            |>
        ]
    ];

    debugEcho[response["StatusCode"], "Response status:"];

    If[response["StatusCode"] != 200,
        debugEcho[response["Body"], "Error body:"];
        Return[$Failed]
    ];

    json = ImportString[response["Body"], "RawJSON"];
    debugEcho[json, "Response JSON:"];

    (* Extract pages list *)
    allpages = Lookup[Lookup[json, "query", <||>], "allpages", {}];
    debugEcho[Length[allpages], "Number of pages:"];

    (* Extract just the titles *)
    Lookup[#, "title", Missing[]] & /@ allpages
]

GetAllPages::noconnection = "No active wiki connection. Use ConnectToWiki first.";


(* ========== Get Page Links ========== *)

GetPageLinks[pageName_String, limit_Integer: 500] := Module[
    {response, json, query, links},

    If[$CurrentWikiConnection === None,
        Message[GetPageLinks::noconnection];
        Return[$Failed]
    ];

    debugEcho[pageName, "Fetching links for page:"];

    response = URLRead[
        HTTPRequest[$CurrentWikiConnection["APIEndpoint"],
            <|"Method" -> "GET",
              "Query" -> {
                  "action" -> "query",
                  "titles" -> pageName,
                  "prop" -> "links",
                  "pllimit" -> ToString[limit],
                  "format" -> "json"
              },
              "Cookies" -> $CurrentWikiConnection["Cookies"]
            |>
        ]
    ];

    debugEcho[response["StatusCode"], "Response status:"];

    If[response["StatusCode"] != 200,
        debugEcho[response["Body"], "Error body:"];
        Return[$Failed]
    ];

    json = ImportString[response["Body"], "RawJSON"];
    debugEcho[json, "Response JSON:"];

    (* Extract pages from response *)
    query = Lookup[json, "query", <||>];
    If[Length[query] == 0, Return[{}]];

    (* Get first (and only) page data *)
    query = Lookup[query, "pages", <||>];
    If[Length[query] == 0, Return[{}]];

    query = First[Values[query]];
    debugEcho[query, "Page data:"];

    (* Extract links *)
    links = Lookup[query, "links", {}];
    debugEcho[Length[links], "Number of links:"];

    (* Extract just the titles *)
    Lookup[#, "title", Missing[]] & /@ links
]

GetPageLinks::noconnection = "No active wiki connection. Use ConnectToWiki first.";

(* edit/upload pages *)
UploadPage[pageName_String, newContent_String, overwritePage_]:= Module[
    {response, json, editResult},
    If[$CurrentWikiConnection === None,
        Message[uploadPage::noconnection];
        Return[$Failed]
    ];

    Echo[pageName, "Uploading page:"];
    Echo[If[overwritePage, "Overwrite", "Create"], "Upload mode:"];

    response = URLRead[
        HTTPRequest[$CurrentWikiConnection["APIEndpoint"],
            <|"Method" -> "POST",
              "Query" -> {
                  "action" -> "edit",
                  "format" -> "json"
              },
              "Body" -> {
                  "token" -> $CurrentWikiConnection["Token"],
                  "text" -> newContent,
                  "title" -> pageName,
                  "contentformat" -> "text/x-wiki",
                  "contentmodel" -> "wikitext",
                  If[overwritePage, Nothing, "createonly" -> "true"]
              }
            |>
        ]
    ];
    response
]

GetImagesForUpload::filenotfound = "Wikitext file not found: `1`";
GetImagesForUpload::nodirectory = "Image directory not found: `1`";

GetImagesForUpload[wikitextFile_String] := Module[
    {wikitextDir, imgDir, wikitextContent, imageRefs, imageFiles, result},

    (* Check if wikitext file exists *)
    If[!FileExistsQ[wikitextFile],
        Message[GetImagesForUpload::filenotfound, wikitextFile];
        Return[$Failed]
    ];

    (* Get directory containing the wikitext file *)
    wikitextDir = DirectoryName[wikitextFile];
    imgDir = FileNameJoin[{wikitextDir, "img"}];

    (* Check if img/ directory exists *)
    If[!DirectoryQ[imgDir],
        Message[GetImagesForUpload::nodirectory, imgDir];
        Return[$Failed]
    ];

    (* Read wikitext content *)
    wikitextContent = Import[wikitextFile, "Text"];

    (* Extract image references from wikitext using WikitextConverter *)
    imageRefs = WolframMediaWiki`WikitextConverter`ExtractImageReferences[wikitextContent];

    (* Strip img/ prefix from references if present *)
    imageRefs = StringReplace[imageRefs, "img/" -> ""];

    (* Find matching files in img/ directory *)
    imageFiles = FileNames["*.png", imgDir];

    (* Build association mapping wiki filename to local file path *)
    result = Association[
        Table[
            With[{filename = FileNameTake[imgFile]},
                (* Check if this image is referenced in the wikitext *)
                If[MemberQ[imageRefs, filename],
                    filename -> imgFile,
                    Nothing
                ]
            ],
            {imgFile, imageFiles}
        ]
    ];

    Echo[Length[result], "Images found for upload:"];
    Echo[Keys[result], "Image filenames:"];

    result
]

PrepareWikitextForUpload[wikitextContent_String] := Module[
    {cleaned},

    (* Replace [[File:img/ with [[File:nb- *)
    cleaned = StringReplace[
        wikitextContent,
        "[[File:img/" -> "[[File:nb-"
    ];

    Echo["Wikitext prepared with nb- prefix", "Info:"];
    cleaned
]


UploadImage::noconnection = "No active wiki connection. Use ConnectToWiki first.";
UploadImage::filenotfound = "Image file not found: `1`";
UploadImage::exists = "File `1` already exists on wiki. Use overwrite->True to replace it.";

UploadImage[imageFilePath_String, wikiFileName_String, overwrite_]:= Module[
    {response, imageData, boundary, body, headers},

    Echo["UploadImage called", "Debug:"];

    (* Check connection *)
    If[$CurrentWikiConnection === None,
        Message[UploadImage::noconnection];
        Return[$Failed]
    ];

    Echo["Connection OK", "Debug:"];

    (* Check if file exists *)
    If[!FileExistsQ[imageFilePath],
        Message[UploadImage::filenotfound, imageFilePath];
        Return[$Failed]
    ];

    Echo["File exists", "Debug:"];

    Echo[wikiFileName, "Uploading image:"];
    Echo[If[overwrite, "Overwrite", "Create only"], "Upload mode:"];

    Echo["Reading image data", "Debug:"];
    imageData = Import[imageFilePath, "String"];
    Echo[StringLength[imageData], "Image data length:"];

    boundary = "----WolframMediaWikiBoundary" <> IntegerString[RandomInteger[10^9]];

    body = StringJoin[
        "--", boundary, "\r\n",
        "Content-Disposition: form-data; name=\"token\"\r\n\r\n",
        $CurrentWikiConnection["Token"], "\r\n",

        "--", boundary, "\r\n",
        "Content-Disposition: form-data; name=\"filename\"\r\n\r\n",
        wikiFileName, "\r\n",

        "--", boundary, "\r\n",
        "Content-Disposition: form-data; name=\"text\"\r\n\r\n",
        "[[Category:BotUpload]]\r\n",

        If[overwrite,
            StringJoin[
                "--", boundary, "\r\n",
                "Content-Disposition: form-data; name=\"ignorewarnings\"\r\n\r\n",
                "1\r\n"
            ],
            ""
        ],

        "--", boundary, "\r\n",
        "Content-Disposition: form-data; name=\"file\"; filename=\"", wikiFileName, "\"\r\n",
        "Content-Type: image/png\r\n\r\n",
        imageData, "\r\n",

        "--", boundary, "--\r\n"
    ];

    headers = {
        "Content-Type" -> "multipart/form-data; boundary=" <> boundary
    };

    response = URLRead[
        HTTPRequest[
            $CurrentWikiConnection["APIEndpoint"] <> "?action=upload&format=json",
            <|
                "Method" -> "POST",
                "Headers" -> headers,
                "Body" -> body
            |>
        ]
    ];

    (* Parse response *)
    If[response["StatusCode"] == 200,
        (* Check for warnings/errors in JSON response *)
        Check[
            With[{json = ImportString[response["Body"], "RawJSON"]},
                If[KeyExistsQ[json, "upload"],
                    If[KeyExistsQ[json["upload"], "result"] && json["upload"]["result"] == "Success",
                        Echo["Success", "Upload result:"];
                        <|"success" -> True, "filename" -> wikiFileName|>,
                        (* Check for warnings *)
                        If[KeyExistsQ[json["upload"], "warnings"],
                            If[KeyExistsQ[json["upload"]["warnings"], "exists"] && !overwrite,
                                Message[UploadImage::exists, wikiFileName];
                                <|"success" -> False, "error" -> "exists", "filename" -> wikiFileName|>,
                                <|"success" -> False, "error" -> "warning", "details" -> json["upload"]["warnings"]|>
                            ],
                            <|"success" -> False, "error" -> "unknown", "response" -> json|>
                        ]
                    ],
                    If[KeyExistsQ[json, "error"],
                        <|"success" -> False, "error" -> json["error"]["code"], "info" -> json["error"]["info"]|>,
                        <|"success" -> False, "error" -> "unknown", "response" -> json|>
                    ]
                ]
            ],
            <|"success" -> False, "error" -> "parse_failed"|>
        ],
        <|"success" -> False, "error" -> "http_error", "status" -> response["StatusCode"]|>
    ]
]

End[]

EndPackage[]
