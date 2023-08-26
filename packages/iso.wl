(* ::Package:: *)

BeginPackage["iso`"];

Needs["utilities`", FileNameJoin[{"packages", "utilities.wl"}]];


T::usage = "";


Begin["`Private`"];


W[k_, d_, \[Chi]_] :=
    Table[
        If[i1 == i2 && j1 == j2 == j3,
            utilities`Weingarten[utilities`GetPermutations[k][[j1]] \[PermutationProduct]
                 InversePermutation[utilities`GetPermutations[k][[i1]]], k, d * \[Chi]^2]
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
    Table[d ^ utilities`CountCycles[\[Sigma] \[PermutationProduct] \[Rho], k], {\[Sigma], utilities`GetPermutations[
        k]}]


Y[k_, \[Chi]_] :=
    Table[\[Chi] ^ utilities`CountCycles[\[Sigma] \[PermutationProduct] InversePermutation[\[Theta]], k], {\[Sigma],
         utilities`GetPermutations[k]}, {\[Theta], utilities`GetPermutations[k]}]


T[\[Rho]_, k_, d_, \[Chi]_] :=
    FullSimplify[Transpose[W[k, d, \[Chi]] . X[\[Rho], k, d] . Y[k, \[Chi]], {1, 2, 
        4, 3}] . Y[k, \[Chi]]]


End[];


EndPackage[];
