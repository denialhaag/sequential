(* ::Package:: *)

BeginPackage["onedimension`"];


ImportFunctions::usage = "";

GetFunctions::usage = "";

SetPerms::usage = "";

CreatePerms::usage = "";

GetPerms::usage = "";

Type::usage = "";

CountCycles::usage = "";

WeingartenFunction::usage = "";

W::usage = "";

X::usage = "";

Y::usage = "";

T::usage = "";


Begin["`Private`"];


ImportFunctions[k_] :=
    (file = FileNameJoin[{NotebookDirectory[], "functions", StringJoin[
        "functions", ToString[k], ".txt"]}]; functions = ToExpression[StringReplace[
        ReadString[file], {"**" -> "^", "]" -> "}", "[" -> "{"}]]; functions)


GetFunctions[k_] :=
    If[ValueQ[Global`Functions],
        If[MemberQ[Keys[Global`Functions], ToString[k]],
            Return[Global`Functions[[ToString[k]]]]
            ,
            Global`Functions = Append[Global`Functions, ToString[k] ->
                 ImportFunctions[k]]; Return[Global`Functions[[ToString[k]]]]
        ]
        ,
        Global`Functions = Association[{ToString[k] -> ImportFunctions[
            k]}]; Return[Global`Functions[[ToString[k]]]]
    ]


SetPerms[k_, list_] :=
    If[ValueQ[Global`Perms],
        If[MemberQ[Keys[Global`Perms], ToString[k]],
            Global`Perms[[ToString[k]]] = list
            ,
            Global`Perms = Append[Global`Perms, ToString[k] -> CreatePerms[
                k]]; Return[Global`Perms[[ToString[k]]]]
        ]
        ,
        Global`Perms = Association[{ToString[k] -> CreatePerms[k]}]; 
            Return[Global`Perms[[ToString[k]]]]
    ]


CreatePerms[k_] :=
    (
        R = Gather[GroupElements[SymmetricGroup[k]], Total[(Map[Length
            ]) @@ #1] == Total[(Map[Length]) @@ #2]&];
        S = {};
        For[i = 1, i <= Length[R], i++,
            AppendTo[S, Sort[R[[i]]]]
        ];
        S = Flatten[S];
        S
    )


GetPerms[k_] :=
    If[ValueQ[Global`Perms],
        If[MemberQ[Keys[Global`Perms], ToString[k]],
            Return[Global`Perms[[ToString[k]]]]
            ,
            Global`Perms = Append[Global`Perms, ToString[k] -> CreatePerms[
                k]]; Return[Global`Perms[[ToString[k]]]]
        ]
        ,
        Global`Perms = Association[{ToString[k] -> CreatePerms[k]}]; 
            Return[Global`Perms[[ToString[k]]]]
    ]


Type[\[Alpha]_, k_] :=
    (
        type = Reverse[Sort[(Map[Length]) @@ \[Alpha]]];
        temp = Total[type];
        For[i = 1, i <= k - temp, i++,
            type = AppendTo[type, 1]
        ];
        type
    )


CountCycles[\[Alpha]_, \[Beta]_, k_] :=
    Length[Type[\[Alpha] \[PermutationProduct] \[Beta], k]]


WeingartenFunction[\[Sigma]_, \[Tau]_, k_] :=
    GetFunctions[k][[Position[GetFunctions[k][[All, 1]], Type[\[Sigma] \[PermutationProduct] InversePermutation[
        \[Tau]], k]][[1, 1]]]][[2]] /. Global`n -> Global`d * Global`\[Chi]


W[k_] :=
    Table[WeingartenFunction[\[Sigma], \[Tau], k], {\[Sigma], GetPerms[k]}, {\[Tau], GetPerms[
        k]}]


X[\[Rho]_, k_] :=
    DiagonalMatrix[Table[Global`d ^ CountCycles[\[Sigma], \[Rho], k], {\[Sigma], GetPerms[
        k]}]]


Y[k_] :=
    Table[Global`\[Chi] ^ CountCycles[\[Sigma], InversePermutation[\[Theta]], k], {\[Sigma], GetPerms[
        k]}, {\[Theta], GetPerms[k]}]


T[\[Rho]_, k_] :=
    FullSimplify[W[k] . X[\[Rho], k] . Y[k]]


End[];


EndPackage[];
