(* ::Package:: *)

BeginPackage["mps`"];

Needs["utilities`", FileNameJoin[{Directory[], "packages", "utilities.wl"
    }]]


T::usage = "";


Begin["`Private`"];


W[k_, d_, \[Chi]_] :=
    Table[Weingarten[\[Sigma] \[PermutationProduct] InversePermutation[\[Tau]], k, d * \[Chi]], {\[Tau], GetPerms[
        k]}, {\[Sigma], GetPerms[k]}]


X[\[Rho]_, k_, d_] :=
    DiagonalMatrix[Table[d ^ CountCycles[\[Sigma] \[PermutationProduct] \[Rho], k], {\[Sigma], GetPerms[k]}]
        ]


Y[k_, \[Chi]_] :=
    Table[\[Chi] ^ CountCycles[\[Sigma] \[PermutationProduct] InversePermutation[\[Theta]], k], {\[Sigma], GetPerms[
        k]}, {\[Theta], GetPerms[k]}]


T[\[Rho]_, k_, d_, \[Chi]_] :=
    FullSimplify[W[k, d, \[Chi]] . X[\[Rho], k, d] . Y[k, \[Chi]]]


End[];


EndPackage[];
