BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Arithmetic`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


makeAddC := makeAddC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_add", {"OpaqueRawPointer", "CUnsignedInt", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"];

makeMultiplyC := makeMultiplyC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_mul", {"OpaqueRawPointer", "CUnsignedInt", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"];

(*
	Z3Plus
*)

Z3Plus[args___Z3ASTObject] :=
	Enclose@Module[{ctx, argArray},
		ctx = Confirm@Z3GetContext[args];
		argArray = RawMemoryExport[#["RawAST"] &/@ {args}, "OpaqueRawPointer"];
		Z3ASTObject[ctx, makeAddC[ctx["RawContext"], Length[{args}], argArray]]
	]


(*
	Z3Times
*)

Z3Times[args___Z3ASTObject] :=
	Enclose@Module[{ctx, argArray},
		ctx = Confirm@Z3GetContext[args];
		argArray = RawMemoryExport[#["RawAST"] &/@ {args}, "OpaqueRawPointer"];
		Z3ASTObject[ctx, makeMultiplyC[ctx["RawContext"], Length[{args}], argArray]]
	]


End[];
EndPackage[];
