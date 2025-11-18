BeginPackage["MediaWikiTools`"];

MediaWikiExportToString;

MediaWikiExportToFile;

Begin["`Private`"];

(* PUBLIC API *)

Options[MediaWikiExportToString] = {"ImageDirectory" -> Automatic};

MediaWikiExportToString[expr_, outputDir_String, opts : OptionsPattern[]] :=
	convertToMediaWiki[expr, outputDir];

MediaWikiExportToFile[expr_, outputFile_String, opts : OptionsPattern[]] :=
	Module[{mediaWikiText, outputDirectory},
		outputDirectory = DirectoryName[outputFile];
		mediaWikiText = MediaWikiExportToString[expr, outputDirectory, opts
			];
		If[StringQ[mediaWikiText],
			Export[outputFile, mediaWikiText, "Text"]
			,
			$Failed
		]
	];

(* Categorize expressions *)

convertToMediaWiki[expression_, imageDirectory_] :=
	cleanupSpecialCharacters @ convertExpressionToMediaWiki[expression, 
		imageDirectory];

convertExpressionToMediaWiki[expression_, imageDirectory_] :=
	Module[{animated, tableData},
		tableData = tableDataFromExpression[expression];
		Which[
			notebookLikeQ[expression],
				convertNotebookToMediaWiki[expression, imageDirectory]
			,
			cellLikeQ[expression],
				convertCellToMediaWiki[expression, imageDirectory]
			,
			tableData =!= $Failed,
				convertTabularToMediaWiki[tableData]
			,
			imageExpressionQ[expression],
				saveImageAndGetWikiLink[ToBoxes[expression], imageDirectory]
			,
			(
					animated = animatedImageFrom[expression];
					animated =!= $Failed
				),
				saveAnimatedImageAndGetWikiLink[animated, imageDirectory]
			,
			hyperlinkQ[expression],
				hyperlinkToMediaWiki[expression]
			,
			StringQ[expression],
				expression
			,
			ListQ[expression],
				convertListToMediaWiki[expression, imageDirectory]
			,
			True,
				convertArbitraryExpression[expression, imageDirectory]
		]
	];

convertNotebookToMediaWiki[notebook_, imageDirectory_] :=
	StringRiffle[
		convertCellToMediaWiki[#, imageDirectory]& /@ extractAllCells[notebook],
		"\n\n"
	];

convertListToMediaWiki[list_List, imageDirectory_] :=
    StringRiffle[convertToMediaWiki[#, imageDirectory] & /@ list, "\n\n"];

convertArbitraryExpression[expression_, imageDirectory_] :=
    saveImageAndGetWikiLink[ToBoxes[expression], imageDirectory];

notebookLikeQ[expression_] := MatchQ[expression, _Notebook | _NotebookObject];
cellLikeQ[expression_] := MatchQ[expression, _Cell];
hyperlinkQ[expression_] := MatchQ[expression, Hyperlink[_, _]];
imageExpressionQ[expression_] := MatchQ[expression, _Image | _Image3D];

hyperlinkToMediaWiki[Hyperlink[label_, url_]] :=
    formatHyperlink[url, ToString[label]];

animatedImageFrom[expression_] /; AnimatedImageQ[expression] := expression;
animatedImageFrom[expression_] /; VideoQ[expression] := AnimatedImage[expression];
animatedImageFrom[expression_] /; MatchQ[expression, _Manipulate] := AnimatedImage[Video[expression]];
animatedImageFrom[___] := $Failed;

convertCellToMediaWiki[Cell[cellContent_, cellStyle_String /; isListItemStyle[cellStyle], ___], imageDirectory_] :=
	convertListItemCell[cellContent, cellStyle];

convertCellToMediaWiki[Cell[cellContent_, cellStyle_String /; isHeaderStyle[cellStyle], ___], _] :=
	formatAsMediaWikiHeader[cellStyle, extractPlainText[cellContent]];

convertCellToMediaWiki[Cell[cellContent_, cellStyle_String /; isSubtitleStyle[cellStyle], ___], _] :=
	convertSubtitleCell[cellContent];

convertCellToMediaWiki[Cell[cellContent_, cellStyle_String /; inputStyleQ[cellStyle], ___], imageDirectory_] :=
	convertInputCell[cellContent, imageDirectory];

convertCellToMediaWiki[cell:Cell[cellContent_, "ExternalLanguage", ___], _] :=
	convertExternalLanguageCell[cell, cellContent];

convertCellToMediaWiki[Cell[cellContent_, "Output", ___], imageDirectory_] :=
	convertOutputCell[cellContent, imageDirectory];

convertCellToMediaWiki[Cell[cellContent_, "Text", ___], _] :=
	extractPlainText[cellContent];

convertCellToMediaWiki[cell_Cell, _] :=
	formatUnknownStyle[cellStyleOf[cell]];

convertListItemCell[cellContent_, cellStyle_] :=
	With[
		{marker = If[isNumberedList[cellStyle], "#", "*"], depth = countSublevels[cellStyle] + 1},
		StringRepeat[marker, depth] <> " " <> extractPlainText[cellContent]
	];

convertSubtitleCell[cellContent_] :=
	"'''" <> extractPlainText[cellContent] <> "'''";

convertInputCell[cellContent_, imageDirectory_] :=
	renderBoxesWithFallback[
		cellContent,
		imageDirectory,
		containsGraphicsOrTable,
		Function[code, formatCodeBlock[code, "mathematica"]]
	];

convertExternalLanguageCell[cell_Cell, cellContent_] :=
	Module[{codeLanguage},
		codeLanguage = extractCodeLanguage[cell];
		formatCodeBlock[convertBoxesToCode[cellContent], codeLanguage]
	];

convertOutputCell[cellContent_, imageDirectory_] :=
	renderBoxesWithFallback[
		cellContent,
		imageDirectory,
		containsGraphics,
		Function[code, "<syntaxhighlight>\n(* " <> code <> " *)\n</syntaxhighlight>"]
	];

renderBoxesWithFallback[cellBoxes_, imageDirectory_, imageTest_, codeFormatter_] :=
	If[TrueQ[imageTest[cellBoxes]],
		saveImageAndGetWikiLink[cellBoxes, imageDirectory],
		codeFormatter[convertBoxesToCode[cellBoxes]]
	];

formatUnknownStyle[style_String] := "<!-- Unknown style: " <> style <> " -->";

cellStyleOf[Cell[_, style_String, ___]] := style;
cellStyleOf[_] := "Text";

isSubtitleStyle[style_String] := MemberQ[{"Subtitle", "Subsubtitle"}, style];
inputStyleQ[style_String] := MemberQ[{"Input", "Code", "Program"}, style];

isListItemStyle[style_String] :=
	StringContainsQ[style, "Item", IgnoreCase->True];

isNumberedList[style_String] :=
	StringContainsQ[style, "Numbered", IgnoreCase->True];

countSublevels[style_String] :=
	StringCount[style, "sub", IgnoreCase->True];

isHeaderStyle[style_String] := KeyExistsQ[$headerLevels, style];

$headerLevels = <|
	"Title" -> 1,
	"Chapter" -> 2,
	"Section" -> 3,
	"Subsection" -> 4,
	"Subsubsection" -> 5,
	"Subsubsubsection" -> 6
|>;

formatAsMediaWikiHeader[headerStyle_String, headerText_String] :=
	With[{headerLevel = Lookup[$headerLevels, headerStyle, 3]},
		With[{equalsSigns = StringRepeat["=", headerLevel]},
			equalsSigns <> " " <> headerText <> " " <> equalsSigns
		]
	];


(* Text extraction from boxes *)
extractPlainText[TextData[boxes_] | BoxData[boxes_]] :=
	convertBoxesToText[boxes];

extractPlainText[plainString_String] :=
	plainString;

convertBoxesToText[StyleBox[content_, styleOptions___]] :=
	applyStyleWrappers[{styleOptions}, convertBoxesToText[content]];

convertBoxesToText[FormBox[boxes_, TraditionalForm]] :=
	"<math>" <> convertBoxesToLaTeX[boxes] <> "</math>";

convertBoxesToText[Cell[BoxData[FormBox[boxes_, TraditionalForm]], ___]] :=
	"<math>" <> convertBoxesToLaTeX[boxes] <> "</math>";

convertBoxesToText[FrameBox[StyleBox[codeContent_, "Code", ___], ___]] :=
	"<code>" <> ToString[codeContent] <> "</code>";

convertBoxesToText[Cell[BoxData[boxes_?isInputForm], ___]] :=
	"<code>" <> convertBoxesToCode[boxes] <> "</code>";

convertBoxesToText[TemplateBox[{label_, {url_, ___}, ___}, "HyperlinkDefault", ___]] :=
	convertBoxesToHyperlink[label, url];

convertBoxesToText[TemplateBox[{label_, url_}, "HyperlinkURL", ___]] :=
	convertBoxesToHyperlink[label, url];

convertBoxesToText[ButtonBox[label_, ___, ButtonData->(pacletPath_String?(StringStartsQ["paclet:"])), ___]] :=
	Module[{wolframDocUrl},
		wolframDocUrl = "https://reference.wolfram.com/language/" <> StringTrim[pacletPath, "paclet:"];
		convertBoxesToHyperlink[label, wolframDocUrl]
	];

convertBoxesToText[ButtonBox[label_, ___, BaseStyle->"Hyperlink", ButtonData->{url_String, ___}, ___]] :=
	convertBoxesToHyperlink[label, url];

convertBoxesToText[Cell[BoxData[boxes_?containsInlineImage], ___]] :=
	saveImageAndGetWikiLink[boxes, $TemporaryDirectory];

convertBoxesToText[RowBox[elementList_]] :=
	StringJoin[convertBoxesToText /@ elementList];

convertBoxesToText[elementList_List] :=
	StringJoin[convertBoxesToText /@ elementList];

convertBoxesToText[plainString_String] :=
	plainString;

convertBoxesToText[InterpretationBox[boxes_, ___]] :=
	convertBoxesToText[boxes];

convertBoxesToText[_] :=
	"";

applyStyleWrappers[styleOptions_List, text_] :=
	Fold[wrapTextWithMarkers, text, styleWrappersFromOptions[styleOptions]];

wrapTextWithMarkers[text_, {prefix_, suffix_}] :=
	prefix <> text <> suffix;

styleWrappersFromOptions[styleOptions_List] :=
	Module[{variations = Cases[styleOptions, FontVariations -> variation_, 1]},
		DeleteCases[
			{
				If[MemberQ[styleOptions, FontWeight->("Bold"|Bold)], {"'''", "'''"}, Nothing],
				If[MemberQ[styleOptions, FontSlant->("Italic"|Italic)], {"''", "''"}, Nothing],
				If[AnyTrue[variations, strikeThroughOptionQ], {"<s>", "</s>"}, Nothing]
			},
			Nothing
		]
	];

strikeThroughOptionQ[variation_] :=
	MatchQ[variation, {(_Rule)...}] && TrueQ @ Lookup[Association @@ variation, "StrikeThrough", False];

convertBoxesToHyperlink[label_, url_] :=
	formatHyperlink[url, convertBoxesToText[label]];



(* Formatters *)
formatHyperlink[url_String, label_String] :=
	"[" <> url <> " " <> label <> "]";

formatCodeBlock[codeText_String, languageName_String] :=
	"<syntaxhighlight lang=\"" <> languageName <> "\">\n" <> codeText <> "\n</syntaxhighlight>";


(* Handle tabular *)
convertTabularToMediaWiki[{columnHeaders_, dataMatrix_}] :=
	Module[{formattedRows},
		If[!MatrixQ[dataMatrix],
			Return["<!-- Failed to convert table -->"]
		];

		formattedRows = formatTableDataRow /@ dataMatrix;

		StringJoin[
			"{| class=\"wikitable\"\n",
			If[ListQ[columnHeaders] && columnHeaders =!= {},
				formatTableHeaderRow[columnHeaders] <> "\n",
				""
			],
			StringRiffle[formattedRows, "\n"],
			"\n|}"
		]
	];

tableDataFromExpression[expression_] :=
	Which[
		MatchQ[expression, _Tabular | _Dataset],
		With[{normalized = safeNormal[expression]},
			If[normalized === $Failed, $Failed, tableDataFromExpression[normalized]]
		],

		MatrixQ[expression],
		{None, expression},

		AssociationQ[expression],
		{{"Key", "Value"}, KeyValueMap[List, expression]},

		MatchQ[expression, {__Association}],
		associationListToTable[expression],

		MatchQ[expression, {(_Rule | _RuleDelayed) ..}],
		{{"Key", "Value"}, List @@@ expression},

		True,
		$Failed
	];

associationListToTable[data_] :=
	With[{headers = DeleteDuplicates @ Flatten[Keys /@ data]},
		If[headers === {},
			$Failed,
			{headers, Lookup[#, headers, ""] & /@ data}
		]
	];

safeNormal[expr_] :=
	Module[{normalized = Quiet @ Check[Normal[expr], $Failed]},
		If[normalized === $Failed || normalized === expr, $Failed, normalized]
	];

formatTableHeaderRow[headers_List] := formatTableRow[headers, "!"];

formatTableDataRow[rowData_List] := formatTableRow[rowData, "|"];

formatTableRow[row_List, marker_String] :=
	"|-\n" <> StringRiffle[(marker <> " " <> ToString[#])& /@ row, "\n"];


(* Handle images *)
saveImageAndGetWikiLink[boxes_, outputDirectory_] :=
	saveMediaWikiAsset[
		boxes, outputDirectory, ".png",
		Function[{imagePath}, Export[imagePath, Cell[boxes, "Output"]]]
	];

saveAnimatedImageAndGetWikiLink[animatedImage_?AnimatedImageQ, outputDirectory_] :=
	saveMediaWikiAsset[
		animatedImage, outputDirectory, ".webp",
		Function[{imagePath}, Export[imagePath, animatedImage, "WebP"]]
	];

saveMediaWikiAsset[data_, outputDirectory_, extension_, exportFunction_] :=
	Module[{imageHash, imageDirectory, imagePath, imageFileName},
		imageHash = Hash[data, "Expression", "Base36String"];
		imageDirectory = FileNameJoin[{outputDirectory, "img"}];
		ensureDirectoryExists[imageDirectory];
		(* Append nb- to all api created files so we can delete them if something goes terribly wrong *)
		imageFileName = "nb-" <> imageHash <> extension;
		imagePath = FileNameJoin[{imageDirectory, imageFileName}];
		If[!FileExistsQ[imagePath],
			Quiet[exportFunction[imagePath]]
		];
		"[[File:" <> imageFileName <> "|thumb|img/" <> imageFileName <> "]]"
	];

ensureDirectoryExists[directory_String] :=
	If[!DirectoryQ[directory],
		CreateDirectory[directory, CreateIntermediateDirectories->True]
	];



(* Detect content in boxes *)
containsGraphics[boxes_] :=
	!FreeQ[boxes, GraphicsBox | Graphics3DBox | DynamicModuleBox | RasterBox];

containsTable[boxes_] :=
	!FreeQ[boxes, _GridBox];

containsGraphicsOrTable[boxes_] :=
	containsGraphics[boxes] || containsTable[boxes];

containsInlineImage[boxes_] :=
	containsGraphics[boxes];

isInputForm[boxes_] :=
	FreeQ[boxes, Except[BoxData | TextData | List | RowBox | SuperscriptBox, _Symbol]];



(* Handle external language cells *)
extractCodeLanguage[Cell[_, "ExternalLanguage", ___, CellEvaluationLanguage->languageName_, ___]] :=
	ToLowerCase[languageName /. "NodeJS"->"javascript"];

extractCodeLanguage[_] :=
	"python";

convertBoxesToCode[BoxData[boxes_]] :=
	Module[{codeText},
		codeText = First[FrontEndExecute[FrontEnd`ExportPacket[BoxData[boxes], "InputText"]]];
		StringReplace[codeText, {"\r\n"->"\n", "\r"->"\n"}]
	];

convertBoxesToCode[plainString_String] :=
	plainString;

convertBoxesToCode[otherExpression_] :=
	ToString[otherExpression, InputForm];

convertBoxesToLaTeX[boxes_] :=
	Quiet[
		Check[
			Convert`TeX`BoxesToTeX[boxes],
			ToString[boxes]
		]
	];

(* Extract cell utilities *)
extractAllCells[nb_NotebookObject] :=
	Module[{notebook = Quiet @ Check[NotebookGet[nb], $Failed]},
		If[MatchQ[notebook, Notebook[__]],
			extractAllCells[notebook],
			{}
		]
	];

extractAllCells[Notebook[cells_List, ___]] :=
	Cases[cells, _Cell, Infinity];

extractAllCells[_] := {};

(* Cleanup functions *)
cleanupSpecialCharacters[text_String] :=
	StringReplace[text, $cleanupReplacementRules];

$cleanupReplacementRules = {
	FromCharacterCode[8232] -> "\n",
	"\\[Rule]" -> "->",
	"\\[RuleDelayed]" -> ":>",
	"\\[LessEqual]" -> "<=",
	"\\[GreaterEqual]" -> ">=",
	"\\[NotEqual]" -> "!=",
	"\\[Equal]" -> "==",
	"\\[Times]" -> "×"
};

End[];
EndPackage[];
