BeginPackage["ChristopherWolfram`Z3Link`FunctionDeclaration`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


makeFuncDecl := makeFuncDecl =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_func_decl",
		{
			"OpaqueRawPointer",
			"OpaqueRawPointer",
			"CUnsignedInt",
			"RawPointer"::["OpaqueRawPointer"],
			"OpaqueRawPointer"
		} -> "OpaqueRawPointer"
	];


(*
	CreateZ3FunctionDeclaration
*)

Options[CreateZ3FunctionDeclaration] = {Z3Context :> $Z3Context};

CreateZ3FunctionDeclaration[declName_Z3SymbolObject, domain:{___Z3SortObject}, range_Z3SortObject, opts:OptionsPattern[]] :=
	With[{
			ctx = OptionValue[Z3Context],
			domainArr = RawMemoryExport[#["RawSort"]&/@domain, "OpaqueRawPointer"]
		},
		Z3FunctionDeclarationObject[ctx, makeFuncDecl[ctx, declName["RawSymbol"], Length[domain], range["RawSort"]]]
	]


(*
	Z3FunctionDeclarationObject
*)

Z3FunctionDeclarationObject[ctx_Z3ContextObject, rawDecl_]["RawFunctionDeclaration"] := rawDecl
Z3FunctionDeclarationObject[ctx_Z3ContextObject, rawDecl_]["Context"] := ctx

Z3FunctionDeclarationObject /: MakeBoxes[decl_Z3FunctionDeclarationObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3FunctionDeclarationObject,
		decl,
		None,
		{BoxForm`SummaryItem@{"raw function declaration: ", decl["RawFunctionDeclaration"]}},
		{},
		form
	]


End[];
EndPackage[];
