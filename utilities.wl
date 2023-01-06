(* ::Package:: *)

BeginPackage["utilities`"];


ImportFunctions::usage = "";

GetFunctions::usage = "";

Type::usage = "";

CreatePerms::usage = "";

GetPerms::usage = "";

CountCycles::usage = "";

Weingarten::usage = "";


Begin["`Private`"];


ImportFunctions[k_] :=
    (file = FileNameJoin[{NotebookDirectory[], "functions", StringJoin[
        "functions", ToString[k], ".txt"]}]; ToExpression[StringReplace[ReadString[
        file], {"**" -> "^", "]" -> "}", "[" -> "{"}]])


GetFunctions[k_] :=
    (
        If[ValueQ[Global`Functions],
            If[MemberQ[Keys[Global`Functions], ToString[k]],
                Null
                ,
                AppendTo[Global`Functions, ToString[k] -> ImportFunctions[
                    k]]
            ]
            ,
            Global`Functions = Association[{ToString[k] -> ImportFunctions[
                k]}]
        ];
        Global`Functions[[ToString[k]]]
    )


Type[\[Alpha]_, k_] :=
    (temp = Reverse[Sort[Map[Length] @@ \[Alpha]]]; PadRight[temp, k - Total[
        temp] + Length[temp], 1])


CreatePerms[k_] :=
    Flatten[GatherBy[SortBy[GroupElements[SymmetricGroup[k]], PermutationLength[
        #]&], {Type[#, k]&, PermutationSupport[#]&}]]


GetPerms[k_] :=
    (
        If[ValueQ[Global`Perms],
            If[MemberQ[Keys[Global`Perms], ToString[k]],
                Null
                ,
                AppendTo[Global`Perms, ToString[k] -> CreatePerms[k]]
                    
            ]
            ,
            Global`Perms = Association[{ToString[k] -> CreatePerms[k]
                }];
        ];
        Global`Perms[[ToString[k]]]
    )


CountCycles[\[Alpha]_, k_] :=
    Length[Type[\[Alpha], k]]


Weingarten[\[Alpha]_, k_, q_] :=
    GetFunctions[k][[Position[GetFunctions[k][[All, 1]], Type[\[Alpha], k]][[1,
         1]]]][[2]] /. Global`n -> q


End[];


EndPackage[];
