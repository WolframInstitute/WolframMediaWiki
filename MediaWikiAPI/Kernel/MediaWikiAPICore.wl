$CurrentWikiConnection = None;

ClearAll[
    normalizeEndpoint,
    loadConfig,
    connectionOrFail,
    wikiRequest,
    requestJSON,
    mergeCookies,
    normalizeCookies,
    wikiTokenRequest,
    refreshConnectionToken,
    ConnectToWiki,
    connectToWikiInternal,
    getLoginToken,
    performLogin,
    getEditToken,
    GetPage,
    GetAllPages,
    GetPageLinks,
    UploadPage,
    GetImagesForUpload,
    PrepareWikitextForUpload,
    UploadImage
];

normalizeEndpoint[url_String] := Module[{trimmed = StringTrim[url]},
    Which[
        StringEndsQ[trimmed, "api.php"], trimmed,
        StringEndsQ[trimmed, "/"], trimmed <> "api.php",
        True, trimmed <> "/api.php"
    ]
];

loadConfig[configFile_String] := Module[{config},
    If[!FileExistsQ[configFile],
        Message[ConnectToWiki::notfound, configFile];
        Return[$Failed]
    ];
    config = Get[configFile];
    If[!AssociationQ[config] || !SubsetQ[{"URL", "Username", "Password"}, Keys[config]],
        Message[ConnectToWiki::invalid];
        Return[$Failed]
    ];
    config
];

connectionOrFail[symbol_Symbol] := Module[{conn = $CurrentWikiConnection},
    If[AssociationQ[conn],
        conn,
        Message[symbol::noconnection];
        $Failed
    ]
];

Options[wikiRequest] = {"URL" -> Automatic, "Cookies" -> Automatic};
wikiRequest[conn_Association, request_Association, opts : OptionsPattern[]] := Module[
    {req = request, url = OptionValue["URL"]},
    url = Replace[url, Automatic :> conn["APIEndpoint"]];
    If[!KeyExistsQ[req, "Cookies"] && OptionValue["Cookies"] =!= None,
        req["Cookies"] = normalizeCookies @ Replace[
            OptionValue["Cookies"],
            Automatic :> Lookup[conn, "Cookies", {}]
        ];
    ];
    URLRead[HTTPRequest[url, req]]
];

Options[requestJSON] = Options[wikiRequest];
requestJSON[conn_Association, request_Association, opts : OptionsPattern[]] := Module[
    {response = wikiRequest[conn, request, opts]},
    {response, If[response["StatusCode"] == 200, ImportString[response["Body"], "RawJSON"], $Failed]}
];

normalizeCookies[cookieData_] := Select[
    Flatten[{cookieData} /. Missing[__] -> {}, Infinity],
    AssociationQ[#] && KeyExistsQ[#, "Name"] &
];

mergeCookies[cookieData_] := Module[
    {normalized = normalizeCookies[cookieData]},
    DeleteDuplicatesBy[
        normalized,
        Function[cookie,
            {
                Lookup[cookie, "Domain", None],
                Lookup[cookie, "Path", None],
                Lookup[cookie, "Name", None]
            }
        ]
    ]
];

wikiTokenRequest[apiEndpoint_String, cookies_, tokenType_String] := Module[
    {requestCookies = normalizeCookies[cookies], response, json, tokenKey, token, mergedCookies},
    response = URLRead[
        HTTPRequest[apiEndpoint,
            <|"Method" -> "GET",
              "Query" -> <|
                  "action" -> "query",
                  "meta" -> "tokens",
                  "type" -> tokenType,
                  "format" -> "json"
              |>,
              "Cookies" -> requestCookies
            |>
        ]
    ];
    If[response["StatusCode"] != 200, Return[$Failed]];
    json = ImportString[response["Body"], "RawJSON"];
    tokenKey = tokenType <> "token";
    token = Lookup[Lookup[Lookup[json, "query", <||>], "tokens", <||>], tokenKey, $Failed];
    If[token === $Failed, Return[$Failed]];
    mergedCookies = mergeCookies[{requestCookies, response["Cookies"]}];
    <|"Token" -> token, "Cookies" -> mergedCookies|>
];

refreshConnectionToken[conn_Association] := Module[{tokenData},
    tokenData = getEditToken[conn["APIEndpoint"], conn["Cookies"]];
    If[tokenData === $Failed, Return[$Failed]];
    Join[conn, <|"Token" -> tokenData["Token"], "Cookies" -> tokenData["Cookies"]|>]
];

ConnectToWiki[configFile_String] := Module[{config},
    config = loadConfig[configFile];
    If[config === $Failed, Return[$Failed]];
    connectToWikiInternal @@ Lookup[config, {"URL", "Username", "Password"}]
];

ConnectToWiki::notfound = "Config file not found: `1`";
ConnectToWiki::invalid = "Config file must be an Association with keys: \"URL\", \"Username\", \"Password\"";
ConnectToWiki::loginfailed = "Login failed: `1`";
ConnectToWiki::tokenfailed = "Failed to obtain edit token";

connectToWikiInternal[url_String, username_String, password_String] := Module[
    {apiEndpoint, loginTokenData, loginResult, editTokenData, connection},
    apiEndpoint = normalizeEndpoint[url];
    loginTokenData = getLoginToken[apiEndpoint];
    If[loginTokenData === $Failed,
        Message[ConnectToWiki::loginfailed, "Could not retrieve login token"];
        Return[$Failed]
    ];
    loginResult = performLogin[
        apiEndpoint,
        username,
        password,
        loginTokenData["Token"],
        loginTokenData["Cookies"]
    ];
    If[loginResult === $Failed,
        Message[ConnectToWiki::loginfailed, "Login failed"];
        Return[$Failed]
    ];
    editTokenData = getEditToken[apiEndpoint, loginResult["Cookies"]];
    If[editTokenData === $Failed,
        Message[ConnectToWiki::tokenfailed];
        Return[$Failed]
    ];
    connection = <|
        "URL" -> url,
        "APIEndpoint" -> apiEndpoint,
        "Username" -> username,
        "Token" -> editTokenData["Token"],
        "Cookies" -> editTokenData["Cookies"]
    |>;
    $CurrentWikiConnection = connection;
    connection
];

getLoginToken[apiEndpoint_String] := wikiTokenRequest[apiEndpoint, {}, "login"];
getEditToken[apiEndpoint_String, cookies_List] := wikiTokenRequest[apiEndpoint, cookies, "csrf"];

performLogin[apiEndpoint_String, username_String, password_String, loginToken_String, initialCookies_ : {}] := Module[
    {requestCookies = normalizeCookies[initialCookies], response, json, loginResult, combinedCookies},
    response = URLRead[
        HTTPRequest[apiEndpoint,
            <|"Method" -> "POST",
              "Body" -> {
                  "action" -> "login",
                  "lgname" -> username,
                  "lgpassword" -> password,
                  "lgtoken" -> loginToken,
                  "format" -> "json"
              },
              "Cookies" -> requestCookies
            |>
        ]
    ];
    If[response["StatusCode"] != 200, Return[$Failed]];
    json = ImportString[response["Body"], "RawJSON"];
    loginResult = Lookup[Lookup[json, "login", <||>], "result", ""];
    If[loginResult != "Success", Return[$Failed]];
    combinedCookies = mergeCookies[{requestCookies, response["Cookies"]}];
    <|"Success" -> True, "Cookies" -> combinedCookies|>
];

GetPage::noconnection = "No active wiki connection. Use ConnectToWiki first.";
GetPage[pageName_String] := Module[
    {conn = connectionOrFail[GetPage], response, json, pages, pageData, revisions},
    If[conn === $Failed, Return[$Failed]];
    {response, json} = requestJSON[conn,
        <|"Method" -> "GET",
          "Query" -> <|
              "action" -> "query",
              "titles" -> pageName,
              "prop" -> "revisions",
              "rvprop" -> "content",
              "format" -> "json"
          |>
        |>
    ];
    If[json === $Failed, Return[$Failed]];
    pages = Lookup[Lookup[json, "query", <||>], "pages", <||>];
    If[Length[pages] == 0, Return[$Failed]];
    pageData = First[Values[pages]];
    revisions = Lookup[pageData, "revisions", {}];
    If[Length[revisions] == 0, Return[$Failed]];
    Lookup[First[revisions], "*", $Failed]
];

GetAllPages::noconnection = "No active wiki connection. Use ConnectToWiki first.";
GetAllPages[limit_Integer : 500] := Module[
    {conn = connectionOrFail[GetAllPages], response, json, allpages},
    If[conn === $Failed, Return[$Failed]];
    {response, json} = requestJSON[conn,
        <|"Method" -> "GET",
          "Query" -> <|
              "action" -> "query",
              "list" -> "allpages",
              "aplimit" -> ToString[limit],
              "format" -> "json"
          |>
        |>
    ];
    If[json === $Failed, Return[$Failed]];
    allpages = Lookup[Lookup[json, "query", <||>], "allpages", {}];
    Lookup[#, "title", Missing["title"]] & /@ allpages
];

GetPageLinks::noconnection = "No active wiki connection. Use ConnectToWiki first.";
GetPageLinks[pageName_String, limit_Integer : 500] := Module[
    {conn = connectionOrFail[GetPageLinks], response, json, pages, pageData, links},
    If[conn === $Failed, Return[$Failed]];
    {response, json} = requestJSON[conn,
        <|"Method" -> "GET",
          "Query" -> <|
              "action" -> "query",
              "titles" -> pageName,
              "prop" -> "links",
              "pllimit" -> ToString[limit],
              "format" -> "json"
          |>
        |>
    ];
    If[json === $Failed, Return[$Failed]];
    pages = Lookup[Lookup[json, "query", <||>], "pages", <||>];
    If[Length[pages] == 0, Return[{}]];
    pageData = First[Values[pages]];
    links = Lookup[pageData, "links", {}];
    Lookup[#, "title", Missing["title"]] & /@ links
];

UploadPage::noconnection = "No active wiki connection. Use ConnectToWiki first.";
UploadPage[pageName_String, newContent_String, overwritePage_] := Module[
    {conn = connectionOrFail[UploadPage], response, refreshed},
    If[conn === $Failed, Return[$Failed]];
    refreshed = refreshConnectionToken[conn];
    If[refreshed === $Failed, Return[$Failed]];
    $CurrentWikiConnection = refreshed;
    response = wikiRequest[refreshed,
        <|"Method" -> "POST",
          "Query" -> <|"action" -> "edit", "format" -> "json"|>,
          "Body" -> DeleteCases[{
              "token" -> refreshed["Token"],
              "text" -> newContent,
              "title" -> pageName,
              "contentformat" -> "text/x-wiki",
              "contentmodel" -> "wikitext",
              If[overwritePage, Nothing, "createonly" -> "true"]
          }, Nothing]
        |>
    ];
    response
];

GetImagesForUpload::filenotfound = "Wikitext file not found: `1`";
GetImagesForUpload::nodirectory = "Image directory not found: `1`";
GetImagesForUpload[wikitextFile_String] := Module[
    {wikitextDir, imgDir, wikitextContent, imageRefs},
    If[!FileExistsQ[wikitextFile],
        Message[GetImagesForUpload::filenotfound, wikitextFile];
        Return[$Failed]
    ];
    wikitextDir = DirectoryName[wikitextFile];
    imgDir = FileNameJoin[{wikitextDir, "img"}];
    If[!DirectoryQ[imgDir],
        Message[GetImagesForUpload::nodirectory, imgDir];
        Return[$Failed]
    ];
    wikitextContent = Import[wikitextFile, "Text"];
    imageRefs = DeleteDuplicates @ StringCases[
        wikitextContent,
        "[[File:" ~~ filename : (WordCharacter | "-" | "_" | ".") .. ~~ "|" :> filename
    ];
    Association @ Cases[
        FileNames["*.png", imgDir],
        file_ /; MemberQ[imageRefs, FileNameTake[file]] :> (FileNameTake[file] -> file)
    ]
];

PrepareWikitextForUpload[wikitextContent_String] :=
    StringReplace[wikitextContent, "[[File:img/" -> "[[File:nb-"];

UploadImage::noconnection = "No active wiki connection. Use ConnectToWiki first.";
UploadImage::filenotfound = "Image file not found: `1`";
UploadImage::exists = "File `1` already exists on wiki. Use overwrite->True to replace it.";
UploadImage[imageFilePath_String, wikiFileName_String, overwrite_] := Module[
    {conn = connectionOrFail[UploadImage], refreshed, elements, response, json, uploadData},
    If[conn === $Failed, Return[$Failed]];
    If[!FileExistsQ[imageFilePath],
        Message[UploadImage::filenotfound, imageFilePath];
        Return[$Failed]
    ];
    refreshed = refreshConnectionToken[conn];
    If[refreshed === $Failed,
        Return[<|"success" -> False, "error" -> "token_failed"|>]
    ];
    $CurrentWikiConnection = refreshed;
    elements = DeleteCases[{
        "token" -> refreshed["Token"],
        "filename" -> wikiFileName,
        "text" -> "[[Category:BotUpload]]",
        If[overwrite, "ignorewarnings" -> "1", Nothing],
        "file" -> File[imageFilePath]
    }, Nothing];
    {response, json} = requestJSON[refreshed,
        <|"Method" -> "POST",
          "Query" -> <|"action" -> "upload", "format" -> "json"|>,
          "Body" -> elements
        |>
    ];
    If[json === $Failed,
        Return[<|"success" -> False, "error" -> "http_error", "status" -> response["StatusCode"]|>]
    ];
    uploadData = Lookup[json, "upload", <||>];
    Which[
        Lookup[uploadData, "result", None] === "Success",
            <|"success" -> True, "filename" -> wikiFileName|>,
        KeyExistsQ[uploadData, "warnings"],
            If[KeyExistsQ[uploadData["warnings"], "exists"] && !overwrite,
                Message[UploadImage::exists, wikiFileName];
                <|"success" -> False, "error" -> "exists", "filename" -> wikiFileName|>,
                <|"success" -> False, "error" -> "warning", "details" -> uploadData["warnings"]|>
            ],
        KeyExistsQ[json, "error"],
            <|"success" -> False, "error" -> json["error"]["code"], "info" -> json["error"]["info"]|>,
        True,
            <|"success" -> False, "error" -> "unknown", "response" -> json|>
    ]
];
