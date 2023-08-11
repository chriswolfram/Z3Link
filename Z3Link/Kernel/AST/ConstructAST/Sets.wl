BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Sets`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


(*
	Z3EmptySet
*)

makeEmptySetC := makeEmptySetC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_empty_set", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

Options[Z3EmptySet] = {Z3Context :> $Z3Context};

Z3EmptySet[sort_Z3SortObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeEmptySetC[ctx["RawContext"], sort["RawSort"]]]
	]


(*
	Z3FullSet
*)

makeFullSetC := makeFullSetC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_empty_set", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

Options[Z3FullSet] = {Z3Context :> $Z3Context};

Z3FullSet[sort_Z3SortObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeFullSetC[ctx["RawContext"], sort["RawSort"]]]
	]


End[];
EndPackage[];
