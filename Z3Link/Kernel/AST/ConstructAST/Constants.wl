BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Constants`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeConstantC := makeConstantC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_const", {"OpaqueRawPointer", "OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

(*
	Z3Constant
*)

Options[Z3Constant] = {Z3Context :> $Z3Context};

Z3Constant[symSpec_Z3SymbolObject, sortSpec_Z3SortObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeConstantC[ctx["RawContext"], symSpec["RawSymbol"], sortSpec["RawSort"]]]
	]


End[];
EndPackage[];
