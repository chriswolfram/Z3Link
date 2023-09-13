BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Quantifiers`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`ToZ3`"]


(*
	Z3DeBruijnIndex
*)

makeBoundC := makeBoundC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_bound", {"OpaqueRawPointer", "CUnsignedInt", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

DeclareFunction[Z3DeBruijnIndex, iZ3DeBruijnIndex, 2];

iZ3DeBruijnIndex[i_Integer, sortSpec_Z3SortObject, opts_] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[sortSpec]},
		Z3ASTObject[ctx, makeBoundC[ctx["RawContext"], i, sortSpec["RawSort"]]]
	]


(*
	Z3ForAll
*)

makeForAllC := makeForAllC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_forall_const",
		{
			"OpaqueRawPointer",                 (* context *)
			"CUnsignedInt",                     (* weight (default 0) *)
			"CUnsignedInt",                     (* number of bound variables *)
			"RawPointer"::["OpaqueRawPointer"], (* array of bound variable ASTs *)
			"CUnsignedInt",                     (* number of patterns *)
			"RawPointer"::["OpaqueRawPointer"], (* array of patterns *)
			"OpaqueRawPointer"                  (* body AST *)
		} -> "OpaqueRawPointer"
	];

DeclareFunction[Z3ForAll, iZ3ForAll, 2];

iZ3ForAll[vars: {varSeq__}, body_, opts_] :=
	Enclose@Module[{ctx, z3Body, z3Vars},
		ctx = Confirm@Z3GetContext[varSeq, body];
		z3Body = Confirm@ToZ3[body, "Boolean", Z3Context -> ctx];
		z3Vars = Confirm@ToZ3[#, Automatic, Z3Context -> ctx]&/@vars;
		Z3ASTObject[ctx,
			makeForAllC[
				ctx["RawContext"],
				0,
				Length[vars],
				RawMemoryExport[#["RawAST"]&/@z3Vars, "OpaqueRawPointer"],
				0,
				RawPointer[0, "OpaqueRawPointer"],
				z3Body["RawAST"]
			]
		]
	]

iZ3ForAll[var_, body_, opts_] :=
	iZ3ForAll[{var}, body, opts]


(*
	Z3Exists
*)

makeExistsC := makeExistsC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_exists_const",
		{
			"OpaqueRawPointer",                 (* context *)
			"CUnsignedInt",                     (* weight (default 0) *)
			"CUnsignedInt",                     (* number of bound variables *)
			"RawPointer"::["OpaqueRawPointer"], (* array of bound variable ASTs *)
			"CUnsignedInt",                     (* number of patterns *)
			"RawPointer"::["OpaqueRawPointer"], (* array of patterns *)
			"OpaqueRawPointer"                  (* body AST *)
		} -> "OpaqueRawPointer"
	];

DeclareFunction[Z3Exists, iZ3Exists, 2];

iZ3Exists[vars: {varSeq__}, body_, opts_] :=
	Enclose@Module[{ctx, z3Body, z3Vars},
		ctx = Confirm@Z3GetContext[varSeq, body];
		z3Body = Confirm@ToZ3[body, "Boolean", Z3Context -> ctx];
		z3Vars = Confirm@ToZ3[#, Automatic, Z3Context -> ctx]&/@vars;
		Z3ASTObject[ctx,
			makeExistsC[
				ctx["RawContext"],
				0,
				Length[vars],
				RawMemoryExport[#["RawAST"]&/@z3Vars, "OpaqueRawPointer"],
				0,
				RawPointer[0, "OpaqueRawPointer"],
				z3Body["RawAST"]
			]
		]
	]

iZ3Exists[var_, body_, opts_] :=
	iZ3Exists[{var}, body, opts]


End[];
EndPackage[];
