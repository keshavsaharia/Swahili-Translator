(* Mathematica Package *)

(* Created by the Wolfram Workbench Mar 14, 2011 *)

BeginPackage["Mfasiri`", {"Msingi`"}]
(* Exported symbols added here with SymbolName::usage *) 

Tafsiri::usage="Tafsiri[text] returns a swahili translation of the input text."

(* For testing only

WordBuffer
CurrentWord
NextWord
NextWords
PeekNextWord
PeekNextWords*)

Begin["`Private`"]
(* Implementation of the package *)

Options[Tafsiri] = {"ShowParseStatus" -> True,  "ShowBuffer" -> True}

Needs["Msingi`"]

CurrentWord
WordBuffer
$ShowBuffer
ParseStatus
$ShowParseStatus

NextWord[] := If[CurrentWord <= Length[WordBuffer],
	word = WordBuffer[[CurrentWord]];
	If[$ShowParseStatus, PrintParse[]];
	CurrentWord++;
	word, ""
]

PeekNextWord[] := If[CurrentWord <= Length[WordBuffer],
	WordBuffer[[CurrentWord]],
	""
]

NextWords[length_] := If[CurrentWord+length-1 <= Length[WordBuffer],
	words = StringJoin[Riffle[Take[WordBuffer, {CurrentWord, CurrentWord+length-1}]," "]];
	CurrentWord+=length;
	words, ""
] 

PeekNextWords[length_] := If[CurrentWord+length-1 <= Length[WordBuffer],
	StringJoin[Riffle[Take[WordBuffer, {CurrentWord, CurrentWord+length-1}]," "]],
	""]

EndQ[word_] := (word=="")

Tafsiri[input_String, OptionsPattern[]]:= 
(	If [ TrueQ[OptionValue["ShowParseStatus"]], $ShowParseStatus = True, $ShowParseStatus = False ];
	If [ TrueQ[OptionValue["ShowBuffer"]], $ShowBuffer = True, $ShowBuffer = False ];
	WordBuffer = ToLowerCase[StringSplit[
					StringReplace[input,{"."->" .","!"->" !","?"->" ?"}]]];
	WordBuffer = RemoveArticles[WordBuffer];
	CurrentWord = 1;
	FormatOutput[ParseSentence[]]
)

FormatOutput[tafsiri_List] :=
(	formatted = StringJoin[Riffle[Flatten[tafsiri]," "]];
	formatted = ToUpperCase[StringTake[formatted,1]] <> StringDrop[formatted,1] <> ".";
	formatted )

ParseSentence[] := Module [ {subject, predicate, conjunction, nextsubject},
	subject = ParseNounPhrase[];
	While[ ConjunctionQ[PeekNextWord[] ],
		conjunction = ToConjunction[NextWord[]];
		nextsubject = ParseNounPhrase[];
		subject = {Flatten[{First[subject], conjunction, First[nextsubject]}], 
				   CombineNounClass[Last[subject], Last[nextsubject]]}];
	predicate = ParseVerbPhrase[];
	predicate = ConjugateVerbs[predicate, Last[subject]];
	
	{First[subject]} ~Join~ predicate
]

CombineNounClass[class1_, class2_] :=
(	If[class1 >= 3 && class1 <16 && class2 >=3 && class2 <16,
		class1+1,
	If[class1 == 101 || class2 == 101, 201,
	If[class1 == 102 || class2 == 102, 202,
	If[class1 == 103 || class2 == 103, 203,
	If[class1 == 104 || class2 == 104, 204,
	If[class1 > 200 || class2 > 200, class1, 2
	]]]]]])

ParseNounPhrase[] := Module [ {subject = {}},
	If [AdjectiveQ[PeekNextWord[]],
		subject = ParseModifiedNoun[],
	If [PossessiveQ[PeekNextWord[]],
		subject = ParsePossessedNoun[],
	If[PronounQ[PeekNextWord[]] && !PronounQ[PeekNextWords[2]],
		subject = Pronoun[NextWord[]],
	If[PronounQ[PeekNextWords[2]],
		subject = Pronoun[NextWords[2]],
	If[NounQ[PeekNextWord[]], subject = Noun[NextWord[]],
	If[RelativeQ[PeekNextWord[]], subject = Relative[NextWord[]]
	]]]]]];
	subject
]

ParseVerbPhrase[] := Module[ {predicate = {}, infinitive},
	If [ PeekNextWord[] == "", predicate = {},
	If [ TenseMarkerQ[ PeekNextWord[]] || VerbQ[ PeekNextWord[]],
		AppendTo[predicate, ParseVerb[]]];
	If [ NounQ [ PeekNextWord[] ],
		AppendTo[predicate, First[ParseNounPhrase[]]],
	If [ AdjectiveQ [ PeekNextWord[] ],
		AppendTo[predicate, ParseModifier[]],
	If [ PossessiveQ [ PeekNextWord[] ],
		AppendTo[predicate, First[ParsePossessedNoun[]]],
	If [PeekNextWords[2]!="" && 
		StringTake[PeekNextWords[2],2] == "to" && VerbQ [ StringDrop[PeekNextWords[2],3] ],
		NextWord[];
		infinitive = Infinitive[NextWord[]];
		If [ StringTake[infinitive, 2] != "ku", infinitive = "ku"<>infinitive ];
		AppendTo[predicate, infinitive],
	If [PeekNextWords[2]!="" && 
		StringTake[PeekNextWords[2],2] == "to" && NounQ [ StringDrop[PeekNextWords[2],3] ],
		NextWord[];
		AppendTo[predicate, First[Noun[NextWord[]]]<>"ni"],
	If[Length [ TakeWhile[PrepositionQ[PeekNextWords[#]] & /@ Range[1,3], TrueQ] ] > 0,
		preposition = Preposition[NextWords[Length [ TakeWhile[PrepositionQ[PeekNextWords[#]] & /@ Range[1,3], TrueQ] ]]];
		subj = First [ ParseNounPhrase[] ];
		If [ preposition == "katika",
			If[ ListQ[subj], 
				subj = Flatten[{First[subj]<>"ni",Rest[subj]}],
				subj = subj <> "ni"
		], AppendTo[predicate, preposition]]; 
		AppendTo[predicate, subj]
	]]]]]]];
	Flatten[predicate]
]

ParseVerb[] := Module [ {tensemarker, verb},
	tensemarker = TenseMarkerQ[PeekNextWords[#]] & /@ Range[1,3];
	If [ Length [ TakeWhile[tensemarker, TrueQ] ] > 0,
		tensemarker = TenseMarker[NextWords[ Length [ TakeWhile[tensemarker, TrueQ] ] ]],
		tensemarker = ""
	];
	If [VerbQ [ PeekNextWord[] ],
		verb = Verb [ NextWord[] ];
		If [ ListQ[tensemarker], 
			verb = {First[verb], Last[tensemarker]}];
		prefix = VerbPrefix[verb[[2]]];
		suffix = Last[prefix]; prefix = First[prefix];
		verb = prefix <> StringDrop[First[verb], -1 * StringLength[suffix]] <> suffix,
		verb = First[tensemarker]
	];
	verb
]

ParseModifiedNoun[] := Module [ {modifier, noun},
	modifier = ParseModifier[];
	noun = Noun[NextWord[]];
	modifier = Map[StringReplace[#, {"%a" -> Marker[Last[noun], "%a"], 
									 "%A" -> Marker[Last[noun], "%A"]}] &, modifier ];
	{Flatten[{First[noun], modifier}],Last[noun]}
]

ParsePossessedNoun[] := Module [ {possess, subj},
	possess = NextWord[];
	If[possess == "your" && PluralEnglishWordQ[PeekNextWord[]],
		possess = Possessive[possess, True],
		possess = Possessive[possess]
	];
	subj = ParseNounPhrase[];
	possess = Marker[Last[subj], "%p"] <> possess;
	{Flatten[{First[subj], possess}], Last[subj]}
]

ParseModifier[] := Module [ {modifier = {}},
	If[ AdjectiveQ[PeekNextWord[]],
		modifier = {Adjective[NextWord[]]};
		While[AdjectiveQ[PeekNextWord[]] || ConjunctionQ[PeekNextWord[]],
			If [ AdjectiveQ[PeekNextWord[]],
				AppendTo[modifier,Adjective[NextWord[]]],
				AppendTo[modifier,ToConjunction[NextWord[]]]
			]
	]];
	modifier
]

ConjugateVerbs[predicate_, subjectclass_] := Module [ {conjugate}, 
	conjugate = Select[ predicate, !TenseQ[#] &];
	conjugate = Flatten[Map[StringReplace[#, {"%v" -> Marker[subjectclass,"%v"],"%n" -> Marker[subjectclass,"%n"],
											  "%a" -> Marker[subjectclass,"%a"],"%A" -> Marker[subjectclass,"%A"]}] &, conjugate]];
	conjugate
]

RemoveArticles[text_List] := Select[text, !MemberQ[{"a","an","the"}, #] &];

(* Lookup functions *)

PluralEnglishWordQ[word_] :=
(	If[AdjectiveQ[word],
	(* a qualified word *)	
		temp = CurrentWord+1;
		While [ AdjectiveQ[ WordBuffer[[temp]] ], temp++];
		PluralEnglishWordQ[ WordBuffer[[temp]] ],
	(* else *)
		plural = False;
		If[ StringTake[word,-1]=="s",
			plural = NounQ[StringDrop[word,-1]]];
		If[!plural && StringTake[word,-2]=="es",
			plural = NounQ[StringDrop[word,-2]]];
		plural
	]
)

PossessiveEnglishWordQ[word_] := False

PrintParse[] :=
(	Print[" Currently in "<>ParseStatus<>"."];
	Print[ToString[WordBuffer]];
	Print[ToString[WordBuffer[[CurrentWord]]]]; )

End[]

EndPackage[]

