BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Quantifiers`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


(*
	Z3DeBruijnIndex
*)

makeBoundC := makeBoundC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_bound", {"OpaqueRawPointer", "CUnsignedInt", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

Z3DeBruijnIndex[i_Integer, sortSpec_Z3SortObject] :=
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

Z3ForAll[vars: {varSeq__Z3ASTObject}, body_Z3ASTObject] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[varSeq, body]},
		Z3ASTObject[ctx,
			makeForAllC[
				ctx["RawContext"],
				0,
				Length[vars],
				RawMemoryExport[#["RawAST"]&/@vars, "OpaqueRawPointer"],
				0,
				RawPointer[0, "OpaqueRawPointer"],
				body["RawAST"]
			]
		]
	]


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

Z3Exists[vars: {varSeq__Z3ASTObject}, body_Z3ASTObject] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[varSeq, body]},
		Z3ASTObject[ctx,
			makeExistsC[
				ctx["RawContext"],
				0,
				Length[vars],
				RawMemoryExport[#["RawAST"]&/@vars, "OpaqueRawPointer"],
				0,
				RawPointer[0, "OpaqueRawPointer"],
				body["RawAST"]
			]
		]
	]


End[];
EndPackage[];
