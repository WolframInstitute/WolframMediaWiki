(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["MediaWikiTools`"];

Get[FileNameJoin[{DirectoryName[$InputFileName], "MediaWikiExport.wl"}]];

(* ::Section:: *)
(* Usage *)
(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

MediaWikiExportToString::usage = "MediaWikiExportToString[\"expr\", \"outputDirName\", \"opts\"] exports given \"expr\" to MediaWiki string.";
MediaWikiExportToFile::usage = "MediaWikiExportToFile[\"expr\", \"outputFileName\", \"opts\"] exports given \"expr\" to MediaWiki file.";

Begin["`Private`"];

End[];
EndPackage[];
