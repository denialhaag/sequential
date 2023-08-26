(* ::Package:: *)

BeginPackage["mps`"];

Needs["utilities`", FileNameJoin[{"packages", "utilities.wl"}]];


T::usage = "";


Begin["`Private`"];


W[k_, d_, \[Chi]_] :=
    Table[utilities`Weingarten[\[Sigma] \[PermutationProduct] InversePermutation[\[Tau]], k, d * \[Chi]], 
        {\[Tau], utilities`GetPermutations[k]}, {\[Sigma], utilities`GetPermutations[k]}]


X[\[Rho]_, k_, d_] :=
    DiagonalMatrix[Table[d ^ utilities`CountCycles[\[Sigma] \[PermutationProduct] \[Rho], k], {\[Sigma], utilities`GetPermutations[
        k]}]]


Y[k_, \[Chi]_] :=
    Table[\[Chi] ^ utilities`CountCycles[\[Sigma] \[PermutationProduct] InversePermutation[\[Theta]], k], {\[Sigma],
         utilities`GetPermutations[k]}, {\[Theta], utilities`GetPermutations[k]}]


T[\[Rho]_, k_, d_, \[Chi]_] :=
    FullSimplify[W[k, d, \[Chi]] . X[\[Rho], k, d] . Y[k, \[Chi]]]


End[];


EndPackage[];
