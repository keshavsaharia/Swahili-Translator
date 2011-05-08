(* Mathematica package *)

BeginPackage["CreateMsingi`", {"DatabaseLink`"}]

CreateMsingi::usage = "CreateMsingi creates the Msingi Swahili database."

Begin["`Private`"]

Needs["DatabaseLink`"]

$DatabaseDirectory = FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Database"}]<>"/MsingiDatabase"
$DataDirectory = FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Data"}] <> "/"

MsingiLink

CreateMsingi[] := Module [ { dataFiles },
 	Print [ "Creating the Msingi HSQL Standalone database." ];
	MsingiLink = OpenSQLConnection[ JDBC["HSQL(Standalone)", $DatabaseDirectory],
									"Name" -> "MsingiDatabase", "Username" -> "MsingiUser" ];
	Print [ "HSQL database successfully created."];
	SetDirectory [ $DataDirectory ];
	dataFiles = FileNames["*.txt"];
	Print [ "Found "<>ToString[Length[dataFiles]] <> " tables suitable for entry into the HSQL database." ];
	Map[ InsertFile[#] &, dataFiles];
	Print [ "All tables inserted." ];
	Print [ "Cleaning up... " ];
	ResetDirectory[];
	Print [ "Database successfully created with 0 errors and 0 warnings." ];
]

InsertFile [ file_ ] :=
(	temp = Import[file, "Table"];
	columns = First[temp];
	temp = Drop[temp, 1];
	temp = FormatTable[temp];
	SQLCreateTable[MsingiLink, SQLTable[StringDrop[file,4]],
		SQLColumn[#, "DataTypeName" -> If [ # != "CLASS", "VARCHAR", "SMALLINT"] ] & /@ columns];
	SQLInsert[MsingiLink, SQLTable[StringDrop[file,4]], columns, temp];
)



FormatTable[table_] := DeleteCases[FormatRow /@ table, ""]
FormatRow[row_List] := "" /; First[row]=="%%" || Length[row] == 1
FormatRow[row_List] := If[StringQ[#], StringReplace[#, {"-" -> "", "_"->" "}], #] & /@ row /; First[row]
FormatRow[row_] := ""

GetMsingiLink[] := OpenSQLConnection[ JDBC["HSQL(Standalone)", $DatabaseDirectory], "Username" -> "MsingiUser" ]

End[]

EndPackage[]