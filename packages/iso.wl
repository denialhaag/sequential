(* ::Package:: *)

BeginPackage["iso`"];

Needs["utilities`", FileNameJoin[{Directory[], "packages", "utilities.wl"
    }]]


T::usage = "";


Begin["`Private`"];


W[k_, d_, \[Chi]_] :=
    Table[
        If[i1 == i2 && j1 == j2 == j3,
            Weingarten[GetPerms[k][[j1]] \[PermutationProduct] InversePermutation[GetPerms[
                k][[i1]]], k, d * \[Chi]^2]
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


X[\[Rho]_, k_, d_] :=
    Table[d ^ CountCycles[\[Sigma] \[PermutationProduct] \[Rho], k], {\[Sigma], GetPerms[k]}]


Y[k_, \[Chi]_] :=
    Table[\[Chi] ^ CountCycles[\[Sigma] \[PermutationProduct] InversePermutation[\[Theta]], k], {\[Sigma], GetPerms[
        k]}, {\[Theta], GetPerms[k]}]


T[\[Rho]_, k_, d_, \[Chi]_] :=
    FullSimplify[Transpose[W[k, d, \[Chi]] . X[\[Rho], k, d] . Y[k, \[Chi]], {1, 2, 
        4, 3}] . Y[k, \[Chi]]]


End[];


EndPackage[];
