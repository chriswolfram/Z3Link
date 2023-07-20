BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Constants`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


makeConstantC := makeConstantC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_const", {"OpaqueRawPointer", "OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

(*
	Z3Constant
*)

Z3Constant[symSpec_Z3SymbolObject, sortSpec_Z3SortObject] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[symSpec, sortSpec]},
		Z3ASTObject[ctx, makeConstantC[ctx["RawContext"], symSpec["RawSymbol"], sortSpec["RawSort"]]]
	]


End[];
EndPackage[];
