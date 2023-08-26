(* ::Package:: *)

BeginPackage["utilities`"];


CountCycles::usage = "";

GetPermutations::usage = "";

Weingarten::usage = "";


Begin["`Private`"];


ImportFunctions[k_] :=
    (file = FileNameJoin[{"functions", StringJoin["functions", ToString[
        k], ".txt"]}]; ToExpression[StringReplace[ReadString[file], {"**" -> 
        "^", "]" -> "}", "[" -> "{", "n" -> "utilities`Private`n"}]])


GetFunctions[k_] :=
    (
        If[ValueQ[utilities`Private`Functions],
            If[MemberQ[Keys[utilities`Private`Functions], ToString[k]
                ],
                Null
                ,
                AppendTo[utilities`Private`Functions, ToString[k] -> 
                    ImportFunctions[k]]
            ]
            ,
            utilities`Private`Functions = Association[{ToString[k] ->
                 ImportFunctions[k]}]
        ];
        utilities`Private`Functions[[ToString[k]]]
    )


Type[\[Alpha]_, k_] :=
    (temp = Reverse[Sort[Map[Length] @@ \[Alpha]]]; PadRight[temp, k - Total[
        temp] + Length[temp], 1])


CreatePermutations[k_] :=
    Flatten[GatherBy[SortBy[GroupElements[SymmetricGroup[k]], PermutationLength[
        #]&], {PermutationSupport[#]&, Type[#, k]&}]]


GetPermutations[k_] :=
    (
        If[ValueQ[utilities`Private`Permutations],
            If[MemberQ[Keys[utilities`Private`Permutations], ToString[
                k]],
                Null
                ,
                AppendTo[utilities`Private`Permutations, ToString[k] 
                    -> CreatePermutations[k]]
            ]
            ,
            utilities`Private`Permutations = Association[{ToString[k]
                 -> CreatePermutations[k]}];
        ];
        utilities`Private`Permutations[[ToString[k]]]
    )


CountCycles[\[Alpha]_, k_] :=
    Length[Type[\[Alpha], k]]


Weingarten[\[Alpha]_, k_, q_] :=
    GetFunctions[k][[Position[GetFunctions[k][[All, 1]], Type[\[Alpha], k]][[1,
         1]]]][[2]] /. utilities`Private`n -> q


End[];


EndPackage[];
