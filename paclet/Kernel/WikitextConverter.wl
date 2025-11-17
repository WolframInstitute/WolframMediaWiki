BeginPackage["WolframMediaWiki`WikitextConverter`"]

NotebookToWikitext::usage = "NotebookToWikitext[notebook] converts a Wolfram Notebook to MediaWiki wikitext format. Returns an Association with keys \"wikitext\" (the converted text) and \"images\" (list of image file paths)."

NotebookToMarkdown::usage = "NotebookToMarkdown[notebook, outputPath] exports a notebook to markdown format with images."

MarkdownToWikitext::usage = "MarkdownToWikitext[markdownFile, wikitextFile] converts a markdown file to MediaWiki format using Pandoc."

ExtractImageReferences::usage = "ExtractImageReferences[wikitextContent] extracts all image file references from wikitext content."

CleanImagePaths::usage = "CleanImagePaths[wikitextContent] removes img/ directory prefixes from image references in wikitext."

Begin["`Private`"]

NotebookToMarkdown[nb_NotebookObject, outputPath_String] := Module[
    {dir, filename, result},

    dir = DirectoryName[outputPath];
    If[dir != "" && !DirectoryQ[dir], CreateDirectory[dir]];

    result = Export[outputPath, nb, "MarkDown"];

    If[result === $Failed,
        Message[NotebookToMarkdown::failed, outputPath];
        $Failed,
        result
    ]
]

NotebookToMarkdown[nbPath_String, outputPath_String] := Module[
    {nb},
    nb = NotebookOpen[nbPath, Visible -> False];
    If[nb === $Failed,
        Message[NotebookToMarkdown::notfound, nbPath];
        $Failed,
        Block[{result = NotebookToMarkdown[nb, outputPath]},
            NotebookClose[nb];
            result
        ]
    ]
]

NotebookToMarkdown::failed = "Failed to export notebook to `1`.";
NotebookToMarkdown::notfound = "Notebook file not found: `1`.";


MarkdownToWikitext[markdownFile_String, wikitextFile_String] := Module[
    {result, dir},

    If[!FileExistsQ[markdownFile],
        Message[MarkdownToWikitext::notfound, markdownFile];
        Return[$Failed]
    ];

    dir = DirectoryName[wikitextFile];
    If[dir != "" && !DirectoryQ[dir], CreateDirectory[dir]];

    result = RunProcess[
        {"pandoc", markdownFile, "-f", "markdown", "-t", "mediawiki", "-o", wikitextFile},
        ProcessDirectory -> DirectoryName[markdownFile]
    ];

    If[result["ExitCode"] != 0,
        Message[MarkdownToWikitext::pandocfailed, result["StandardError"]];
        $Failed,
        wikitextFile
    ]
]

MarkdownToWikitext::notfound = "Markdown file not found: `1`.";
MarkdownToWikitext::pandocfailed = "Pandoc conversion failed: `1`";


ExtractImageReferences[wikitextContent_String] := Module[
    {pattern, matches},
    pattern = RegularExpression["\\[\\[File:([^\\]|]+)"];
    matches = StringCases[wikitextContent, pattern :> "$1"];
    DeleteDuplicates[matches]
]


CleanImagePaths[wikitextContent_String] := Module[
    {cleaned},

    cleaned = StringReplace[
        wikitextContent,
        "[[File:img/" -> "[[File:"
    ];

    cleaned
]


NotebookToWikitext[nb_NotebookObject] := Module[
    {tempDir, mdFile, wikiFile, mdResult, wikiResult, wikitextContent,
     imageRefs, imagePaths, cleanedWikitext},

    tempDir = CreateDirectory[];
    mdFile = FileNameJoin[{tempDir, "temp.md"}];
    wikiFile = FileNameJoin[{tempDir, "temp.wiki"}];

    mdResult = NotebookToMarkdown[nb, mdFile];

    If[mdResult === $Failed,
        DeleteDirectory[tempDir, DeleteContents -> True];
        Return[$Failed]
    ];

    wikiResult = MarkdownToWikitext[mdFile, wikiFile];

    If[wikiResult === $Failed,
        DeleteDirectory[tempDir, DeleteContents -> True];
        Return[$Failed]
    ];

    wikitextContent = Import[wikiFile, "Text"];

    imageRefs = ExtractImageReferences[wikitextContent];

    imagePaths = FileNames["*.png", FileNameJoin[{tempDir, "img"}]];

    cleanedWikitext = CleanImagePaths[wikitextContent];

    <|
        "wikitext" -> cleanedWikitext,
        "images" -> imagePaths,
        "imageReferences" -> imageRefs,
        "tempDirectory" -> tempDir
    |>
]

NotebookToWikitext[nbPath_String] := Module[
    {nb, result},
    nb = NotebookOpen[nbPath, Visible -> False];
    If[nb === $Failed,
        Message[NotebookToWikitext::notfound, nbPath];
        $Failed,
        Block[{res = NotebookToWikitext[nb]},
            NotebookClose[nb];
            res
        ]
    ]
]

NotebookToWikitext::notfound = "Notebook file not found: `1`.";

End[]

EndPackage[]
