(* ::Package:: *)

BeginPackage["mps`", {"utilities`"}];


W::usage = "";

X::usage = "";

Y::usage = "";

T::usage = "";


Begin["`Private`"];


W[k_] :=
    Table[Weingarten[\[Sigma] \[PermutationProduct] InversePermutation[\[Tau]], k, Global`d * Global`\[Chi]
        ], {\[Tau], GetPerms[k]}, {\[Sigma], GetPerms[k]}]


X[\[Rho]_, k_] :=
    DiagonalMatrix[Table[Global`d ^ CountCycles[\[Sigma] \[PermutationProduct] \[Rho], k], {\[Sigma], GetPerms[
        k]}]]


Y[k_] :=
    Table[Global`\[Chi] ^ CountCycles[\[Sigma] \[PermutationProduct] InversePermutation[\[Theta]], k], {\[Sigma], GetPerms[
        k]}, {\[Theta], GetPerms[k]}]


T[\[Rho]_, k_] :=
    FullSimplify[W[k] . X[\[Rho], k] . Y[k]]


End[];


EndPackage[];
