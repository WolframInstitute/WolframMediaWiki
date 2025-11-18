

BeginPackage["MediaWikiAPI`"];

ConnectToWiki::usage = "ConnectToWiki[configFile] loads credentials from a config file and establishes a connection to a MediaWiki instance. Returns an Association with connection properties. The config file should return an Association with keys: \"URL\" (base wiki URL without /api.php), \"Username\", \"Password\".";

$CurrentWikiConnection::usage = "$CurrentWikiConnection is the currently active MediaWiki connection Association.";

UploadImage::usage = "UploadImage[imageFile, wikiFileName] uploads an image file to the connected wiki with the specified filename.";

UploadPage::usage = "UploadPage[pageName, newContent, overwritePage] uploads new content to the specified wiki page.";

GetPage::usage = "GetPage[pageName] retrieves the current content of a wiki page.";

GetAllPages::usage = "GetAllPages[] retrieves a list of all page titles in the wiki. Use GetAllPages[limit] to limit the number of results.";

GetPageLinks::usage = "GetPageLinks[pageName] retrieves all internal wiki links from a page. Use GetPageLinks[pageName, limit] to limit the number of results.";

GetImagesForUpload::usage = "GetImagesForUpload[wikitextFile] reads a wikitext file and its img/ subdirectory, returning an Association mapping wiki filenames to local file paths for manual upload.";

Begin["`Private`"];

Get[FileNameJoin[{DirectoryName[$InputFileName], "MediaWikiAPICore.wl"}]];

End[];
EndPackage[];
