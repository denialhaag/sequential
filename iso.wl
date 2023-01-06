(* ::Package:: *)

BeginPackage["iso`", {"utilities`"}];


W::usage = "";

X::usage = "";

Y::usage = "";

T::usage = "";


Begin["`Private`"];


W[k_] :=
    Table[
        If[i1 == i2 && j1 == j2 == j3,
            Weingarten[GetPerms[k][[j1]] \[PermutationProduct] InversePermutation[GetPerms[
                k][[i1]]], k, Global`d * Global`\[Chi]^2]
            ,
            0
        ]
        ,
        {i1, 1, k!}
        ,
        {i2, 1, k!}
        ,
        {j1, 1, k!}
        ,
        {j2, 1, k!}
        ,
        {j3, 1, k!}
    ]


X[\[Rho]_, k_] :=
    Table[Global`d ^ CountCycles[\[Sigma] \[PermutationProduct] \[Rho], k], {\[Sigma], GetPerms[k]}]


Y[k_] :=
    Table[Global`\[Chi] ^ CountCycles[\[Sigma] \[PermutationProduct] InversePermutation[\[Theta]], k], {\[Sigma], GetPerms[
        k]}, {\[Theta], GetPerms[k]}]


T[\[Rho]_, k_] :=
    FullSimplify[Transpose[W[k] . X[\[Rho], k] . Y[k], {1, 2, 4, 3}] . Y[k
        ]]


End[];


EndPackage[];
