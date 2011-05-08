(* Mathematica Package *)

(* Created by the Wolfram Workbench Mar 14, 2011 *)

BeginPackage["Msingi`", {"DatabaseLink`"}]
(* Exported symbols added here with SymbolName::usage *) 

(*
Boolean Function    Translation Function      *)
PronounQ			Pronoun
PossessiveQ			Possessive
NounQ				Noun
VerbQ				Verb
AdjectiveQ			Adjective
RelativeConnectorQ	RelativeConnector
TenseMarkerQ		TenseMarker
ConjunctionQ		ToConjunction
InfinitiveQ			Infinitive
PrepositionQ		Preposition
PastTenseVerbQ		PastTenseVerb
PastParticipleVerbQ	PastParticipleVerb
PresentIndefiniteVerbQ	PresentIndefiniteVerb
PresentContinuousVerbQ	PresentContinuousVerb
PossessiveVerbQ
VerbPrefix
TenseQ

Marker
VerbPrefix

MsingiLink
GetMsingiLink

CreateMsingiDatabase
UpdateMsingiDatabase
InsertDataFile

Begin["`Private`"]
(* Implementation of the package *)

Needs["DatabaseLink`"]

$DataDirectory = FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Data"}] <> "/"
$DatabaseDirectory = "/Users/Keshav/Desktop/Swahili/Msingi50311/Msingi"
(*$DatabaseDirectory = FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Database"}]*)

$SwahiliQ = {PronounQ, PossessiveQ, NounQ, VerbQ, AdjectiveQ, RelativeConnectorQ, ConjunctionQ}

(* Data tables *)

MsingiLink

Marker[class_, macro_] :=
	First[Flatten[SQLSelect[MsingiLink, "AgreementChart", 
		{StringReplace[macro, {"%a"->"AdjPrefix1", "%A"->"AdjPrefix2", "%v"->"VerbPrefix", "%n"->"VerbNegPrefix",
						  	   "%m"->"ObjectInfix", "%o"->"Of", "%p"->"PossessivePrefix", "%w"->"Which", 
						  	   "%r"->"AnyQuantifier", "%R"->"AllQuantifier", "%"~~__->"AdjPrefix1"}]},
		  SQLStringMatchQ[SQLColumn["Class"],ToString[ToExpression[class]]]]]]

Marker[___] := ""

PronounQ[text_String] := 
	Length[SQLSelect[MsingiLink, "Pronouns", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0
	
Pronoun[text_String] := 
(	swah = Flatten [ SQLSelect[MsingiLink, "Pronouns", {"Swahili", "Class"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]] ];
	If[Length[swah] == 0, text, swah ]
)

PossessiveQ[text_String] := 
	Length[SQLSelect[MsingiLink, "Possessives", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0
	
Possessive[text_String, pluralQ_:False] := 
(	swah = Flatten[ SQLSelect[MsingiLink, "Possessives", {"Swahili"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]]];
	If[Length[swah] == 0, text,
	If[pluralQ, First[swah], Last[swah] ]]
)

RelativeConnectorQ[text_String] := 
	Length[SQLSelect[MsingiLink, "RelativeConnectors", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0
	
RelativeConnector[text_String] := 
(	swah = Flatten [ SQLSelect[MsingiLink, "RelativeConnectors", {"Swahili"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]] ];
	If[Length[swah] == 0, text, First[swah] ]
)

NounQ[text_String]:=
	Length[SQLSelect[MsingiLink, "Nouns", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0

Noun[text_String]:=
(	swah = SQLSelect[MsingiLink, "Nouns", {"Swahili", "Class"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]];
	If[Length[swah] == 0, text, First[swah] ]
)

AdjectiveQ[text_String] := 
	Length[SQLSelect[MsingiLink, "Adjectives", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0

Adjective[text_String] := 
(	swah = Flatten [ SQLSelect[MsingiLink, "Adjectives", {"Swahili"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]] ];
	If[Length[swah] == 0, text, First[swah] ]
)

PrepositionQ[text_String] := 
	Length[SQLSelect[MsingiLink, "Prepositions", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0

Preposition[text_String] := 
(	swah = Flatten [ SQLSelect[MsingiLink, "Prepositions", {"Swahili"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]] ];
	If[Length[swah] == 0, text, First[swah] ]
)

ConjunctionQ[text_String] :=
	Length[SQLSelect[MsingiLink, "Conjunctions", {"English"}, SQLStringMatchQ[ SQLColumn["English"], text ] ] ] > 0

ToConjunction[text_String] :=
(	swah = Flatten [ SQLSelect[MsingiLink, "Conjunctions", {"Swahili"}, 
 				SQLStringMatchQ[SQLColumn["English"], text]] ];
	If[Length[swah] == 0, text, First[swah] ]
)

Infinitive[text_] := 
(	swah = Flatten[SQLSelect[MsingiLink, "Infinitives", {"Swahili"}, 
		SQLStringMatchQ[ SQLColumn["English"], text ] ]];
	If [ swah == {}, text, First[swah]] )

Verb[text_String] := 
(	If[InfinitiveQ[text], {Infinitive[ text ],"PC"},
	If[PastTenseVerbQ[text], { PastTenseVerb[text], "PP" },
	If[PastParticipleVerbQ[text], { PastParticipleVerb[text], "PP" },
	If[PresentIndefiniteVerbQ[text], { PresentIndefiniteVerb[text], "PI", "H"},
	If[PresentContinuousVerbQ[text], { PresentContinuousVerb[text], "PC"},
		text]]]]]
)

VerbQ[text_String] := StringLength[text]>=2 && (InfinitiveQ[text] || PastTenseVerbQ[text] || PastParticipleVerbQ[text]
												|| PresentIndefiniteVerbQ[text] || PresentContinuousVerbQ[text] || PossessiveVerbQ[text])

InfinitiveQ[text_] :=
	(Length[SQLSelect[MsingiLink, "Infinitives", {"English"}, 
		SQLStringMatchQ[ SQLColumn["English"], text ] ]] > 0 )

PastTenseVerbQ[text_] :=
	(Length[SQLSelect[MsingiLink, "Infinitives", {"PastTense"}, 
		SQLStringMatchQ[ SQLColumn["PastTense"], text ] ]] > 0 ) ||
	(StringTake[text, -1] == "d" && BaseVerbQ[StringDrop[text,-1]]) ||
	(StringTake[text, -1] == "ed" && BaseVerbQ[StringDrop[text,-2]])
	
PastTenseVerb[text_] :=
(	swah = Flatten [ SQLSelect[MsingiLink, "Infinitives", {"Swahili"}, 
		SQLStringMatchQ[ SQLColumn["PastTense"], text ] ]];
	If[Length[swah] == 0, text, First[swah] ] )

PastParticipleVerbQ[text_] :=
	(Length[SQLSelect[MsingiLink, "Infinitives", {"PastParticiple"}, 
		SQLStringMatchQ[ SQLColumn["PastParticiple"], text ] ]] > 0 )

PastParticipleVerb[text_] :=
(	swah = Flatten [ SQLSelect[MsingiLink, "Infinitives", {"Swahili"}, 
		SQLStringMatchQ[ SQLColumn["PastParticiple"], text ] ]];
	If[Length[swah] == 0, text, First[swah] ] )

PresentIndefiniteVerbQ[text_] := StringLength[text]>=4 && 
							((StringTake[text, -1] == "s" && InfinitiveQ[StringDrop[text,-1]]) ||
							 (StringTake[text, -2] == "es" && InfinitiveQ[StringDrop[text,-2]]))

PresentIndefiniteVerb[text_] := 
	If [StringTake[text, -1] == "s" && InfinitiveQ[StringDrop[text,-1]], Infinitive[StringDrop[text,-1]],
	If [StringTake[text, -2] == "es" && InfinitiveQ[StringDrop[text,-2]], Infinitive[StringDrop[text, -2]],
		text ]]

PresentContinuousVerbQ[text_] := StringLength[text]>=5 && ( (StringTake[text, -3] == "ing" && InfinitiveQ[StringDrop[text,-3]]) ||
							(StringTake[text, -3] == "ing" && InfinitiveQ[StringDrop[text,-3]<>"e"]) ||
							(StringTake[text, -4] == "ning" && InfinitiveQ[StringDrop[text,-4]]))

PresentContinuousVerb[text_] :=
	If [StringTake[text, -3] == "ing" && InfinitiveQ[StringDrop[text,-3]], Infinitive[StringDrop[text,-3]],
	If [StringTake[text, -3] == "ing" && InfinitiveQ[StringDrop[text,-3]<>"e"], Infinitive[StringDrop[text,-3]<>"e"],
	If [StringTake[text, -4] == "ning" && InfinitiveQ[StringDrop[text,-4]], Infinitive[StringDrop[text,-4]],
		text ]]]	

PossessiveVerbQ[text_] := (text == "has" || text=="have" || text=="had")

TenseMarkerQ[text_] :=
	(Length[SQLSelect[MsingiLink, "TenseMarkers", {"English"}, 
		SQLStringMatchQ[ SQLColumn["English"], text ] ]] > 0 )

TenseMarker[text_] :=
(	swah = Flatten [ SQLSelect[MsingiLink, "TenseMarkers", {"Swahili","Tense"}, 
		SQLStringMatchQ[ SQLColumn["English"], text ] ]];
	If[Length[swah] == 0, text, swah ] )

TenseQ[tense_] :=
	(Length[SQLSelect[MsingiLink, "VerbPrefixes", {"Tense"}, 
		SQLStringMatchQ[ SQLColumn["Tense"], tense ] ]] > 0 )

VerbPrefix[tense_] :=
(	prefix = Flatten[SQLSelect[MsingiLink, "VerbPrefixes", {"Prefix", "Suffix"}, 
		SQLStringMatchQ[ SQLColumn["Tense"], tense ] ]];
	If[Length[prefix] ==1, First[prefix], prefix] )

SwahiliHandoutParser[text_] :=
 Flatten[Map[{{#[[1, 1]], #[[2, 1]]}, {#[[1, 2]], #[[2, 2]]}} &, 
   Partition[
    Flatten[Level[#, 1]] & /@ 
     StringReplace[
      StringSplit[
       text, {Whitespace ~~ "[", 
        "]" ~~ Whitespace}], {"[" ~~ swahsing___ ~~ "/" ~~ 
         swahplur___ ~~ "]" -> {swahsing, swahplur}, 
       sing___ ~~ "/" ~~ plur___ -> {sing, plur}}], 2, 2]], 1]
       

CreateMsingiDatabase[] := Module [ { dataFiles },
 	Print [ "Creating the Msingi HSQL Standalone database." ];
 	Print [ "------------------------------------------------------------" ];
 	ClearAll[MsingiLink];
	MsingiLink = OpenSQLConnection[ JDBC["HSQL(Standalone)", $DatabaseDirectory], 
									"Name" -> "MsingiDatabase", "Username" -> "MsingiUser" ];
	Print [ ToString[MsingiLink] ];
	Print [ "HSQL database successfully linked."];
	Print [ "------------------------------------------------------------" ];
	Print [ $DataDirectory ];
	Print [ $DatabaseDirectory ];
	SetDirectory [ $DataDirectory ];
	dataFiles = FileNames["*.txt"];
	Print [ "Found "<>ToString[Length[dataFiles]] <> " tables for entry into the HSQL database." ];
	Map[ InsertDataFile[#] &, dataFiles];
	Print [ "All tables inserted." ];
	Print [ "Cleaning up... " ];
	Print [ "------------------------------------------------------------" ];
	ResetDirectory[];
	Print [ "Database successfully created." ];
]

UpdateMsingiDatabase[] := Module [ { dataFiles },
 	Print [ "Updating the Msingi HSQL Standalone database." ];
 	Print [ "------------------------------------------------------------" ];
	Print [ "Data directory : "<> $DataDirectory ];
	Print [ "Database directory : "<> $DatabaseDirectory ];
	SetDirectory [ $DataDirectory ];
	dataFiles = FileNames["*.txt"];
	Print [ "Found "<>ToString[Length[dataFiles]] <> " tables to update in the HSQL database." ];
	Print [ "------------------------------------------------------------" ];
	Print [ "Reformatting outdated values."];
	Print [ "Reindexed "<> ToString[Total[ SQLDelete[MsingiLink, StringDrop[#,-4]] & /@ dataFiles]] <> " values."];
	Print [ "Re-indexing tables."];
	Pause [ 0.3 ];
	SQLDropTable[MsingiLink, StringDrop[#,-4]] & /@ dataFiles;
	Print [ "------------------------------------------------------------" ];
	Print [ "Hashing and re-entering into database."];
	InsertDataFile[#] & /@ dataFiles;
	Pause [ 0.5 ];
	Print [ "All tables updated." ];
	Print [ "Cleaning up... " ];
	Pause [ 0.1 ];
	Print [ "------------------------------------------------------------" ];
	ResetDirectory[];
	Print [ "Database successfully updated." ];
]

InsertDataFile [ file_ ] := Module [ {columns, temp},
	Print["Inserting "<> StringDrop[file,-4]<>" table."];
	temp = FormatTable[Import[$DataDirectory <> file, "Table"]];
	columns = First[temp];
	temp = Drop[temp, 1];
	SQLCreateTable[MsingiLink, SQLTable[StringDrop[file,-4]],
		SQLColumn[#, "DataTypeName" -> If [ # != "Class", "VARCHAR", "SMALLINT"] ] & /@ columns];
	SQLInsert[MsingiLink, SQLTable[StringDrop[file, -4]], columns, temp];
]

UpdateDataFile [ file_ ] := Module [ {temp, columns},
	Print["Updating "<> StringDrop[file,-4]<>" table."];
	temp = FormatTable[Import[$DataDirectory<>file, "Table"]];
	columns = First[temp];
	temp = Drop[temp, 1];
	SQLUpdate[MsingiLink, SQLTable[StringDrop[file, -4]], columns, temp];
]

FormatTable[table_List] := DeleteCases[FormatRow /@ table, ""]
FormatRow[row_List] := "" /; First[row]=="%%"
FormatRow[row_List] := If[StringQ[#], StringReplace[#, {"-" -> "", "_"->" "}], #] & /@ row

GetMsingiLink[] := OpenSQLConnection[ JDBC["HSQL(Standalone)", "/Users/Keshav/Desktop/Swahili/MsingiDatabase"], "Username" -> "MsingiUser" ]



End[]

EndPackage[]

