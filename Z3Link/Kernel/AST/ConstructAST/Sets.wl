BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Sets`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


(*
	Z3EmptySet
*)

makeEmptySetC := makeEmptySetC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_empty_set", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

DeclareFunction[Z3EmptySet, iZ3EmptySet, 1];

Options[Z3EmptySet] = {Z3Context :> $Z3Context};

iZ3EmptySet[sort_Z3SortObject, opts_] :=
	With[{ctx = OptionValue[Z3EmptySet, opts, Z3Context]},
		Z3ASTObject[ctx, makeEmptySetC[ctx["RawContext"], sort["RawSort"]]]
	]


(*
	Z3FullSet
*)

makeFullSetC := makeFullSetC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_empty_set", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

DeclareFunction[Z3FullSet, iZ3FullSet, 1];

Options[Z3FullSet] = {Z3Context :> $Z3Context};

Z3FullSet[sort_Z3SortObject, opts_] :=
	With[{ctx = OptionValue[Z3FullSet, opts, Z3Context]},
		Z3ASTObject[ctx, makeFullSetC[ctx["RawContext"], sort["RawSort"]]]
	]


End[];
EndPackage[];
