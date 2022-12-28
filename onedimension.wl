(* ::Package:: *)

BeginPackage["onedimension`"];


ImportFunctions::usage = "";

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
        ReadString[file], {"**" -> "^", "]" -> "}", "[" -> "{"}]]; Return[functions
        ])


SetPerms[list_] :=
    Global`Perms = list


CreatePerms[k_] :=
    (
        R = Gather[GroupElements[SymmetricGroup[k]], Total[(Map[Length
            ]) @@ #1] == Total[(Map[Length]) @@ #2]&];
        S = {};
        For[i = 1, i <= Length[R], i++,
            AppendTo[S, Sort[R[[i]]]]
        ];
        S = Flatten[S];
        Return[S]
    )


GetPerms[k_] :=
    If[ValueQ[Global`Perms],
        Return[Global`Perms]
        ,
        Return[CreatePerms[k]]
    ]


Type[\[Alpha]_, k_] :=
    (
        type = Reverse[Sort[(Map[Length]) @@ \[Alpha]]];
        temp = Total[type];
        For[l = 1, l <= k - temp, l++,
            type = AppendTo[type, 1]
        ];
        Return[type]
    )


CountCycles[\[Alpha]_, \[Beta]_, k_] :=
    Return[Length[Type[\[Alpha] \[PermutationProduct] \[Beta], k]]]


WeingartenFunction[\[Sigma]_, \[Tau]_, k_] :=
    (functions = ImportFunctions[k]; Return[functions[[Position[functions
        [[All, 1]], Type[\[Sigma] \[PermutationProduct] InversePermutation[\[Tau]], k]][[1, 1]]]][[2]] /. Global`n
         -> Global`d * Global`\[Chi]])


W[k_] :=
    (perms = GetPerms[k]; Return[Table[\[Sigma] = perms[[i]]; \[Tau] = perms[[j]]
        ; WeingartenFunction[\[Sigma], \[Tau], k] /. Global`n -> Global`d * Global`\[Chi], {i,
         1, k!}, {j, 1, k!}]])


X[\[Rho]_, k_] :=
    (perms = GetPerms[k]; Return[DiagonalMatrix[Table[\[Sigma] = perms[[i]];
         Global`d ^ CountCycles[\[Sigma], \[Rho], k], {i, 1, k!}]]])


Y[k_] :=
    (perms = GetPerms[k]; Return[Table[\[Sigma] = perms[[i]]; \[Theta] = perms[[j]]
        ; Global`\[Chi] ^ CountCycles[\[Sigma], InversePermutation[\[Theta]], k], {i, 1, k!}, {j,
         1, k!}]])


T[\[Rho]_, k_] :=
    Return[FullSimplify[W[k] . X[\[Rho], k] . Y[k]]]


End[];


EndPackage[];
