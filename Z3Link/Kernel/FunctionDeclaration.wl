BeginPackage["ChristopherWolfram`Z3Link`FunctionDeclaration`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


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
	Z3FunctionDeclarationCreate
*)

Z3FunctionDeclarationCreate[declName_Z3SymbolObject, domain:{___Z3SortObject}, range_Z3SortObject, opts:OptionsPattern[]] :=
	Enclose@With[{
			ctx = Confirm@Z3GetContext[declName, Sequence@@domain, range],
			domainArr = If[Length[domain] === 0, OpaqueRawPointer[0], RawMemoryExport[#["RawSort"]&/@domain, "OpaqueRawPointer"]]
		},
		Z3FunctionDeclarationObject[ctx, makeFuncDecl[ctx["RawContext"], declName["RawSymbol"], Length[domain], domainArr, range["RawSort"]]]
	]


(*
	Subvalues (function application)
*)

makeAppC := makeAppC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_app",
		{
			"OpaqueRawPointer",
			"OpaqueRawPointer",
			"CUnsignedInt",
			"RawPointer"::["OpaqueRawPointer"]
		} -> "OpaqueRawPointer"
	];

decl_Z3FunctionDeclarationObject[args___Z3ASTObject] :=
	Enclose@Module[{ctx, argCount, argArr},

		ctx = Confirm@Z3GetContext[decl, args];

		argCount = Information[decl, "ArgumentCount"];
		ConfirmAssert[Length[{args}] === argCount,
			Message[Z3FunctionDeclarationObject::appargcount, argCount, decl, Length[{args}], {args}];
			Failure["ArgumentCount", <|
				"MessageTemplate" :> Z3FunctionDeclarationObject::appargcount,
				"MessageParameters" -> {argCount, decl, Length[{args}], {args}},
				"Arguments" -> {args},
				"FunctionDeclaration" -> decl
			|>]
		];

		ConfirmAssert[AllTrue[Transpose[{Information[decl,"Domain"], {args}}], Apply[#1["Hash"] === #2["Sort"]["Hash"]&]],
			Message[Z3FunctionDeclarationObject::appargsort, Information[decl,"Domain"], decl, #["Sort"]&/@{args}];
			Failure["ArgumentSorts", <|
				"MessageTemplate" :> Z3FunctionDeclarationObject::appargsort,
				"MessageParameters" -> {Information[decl,"Domain"], decl, #["Sort"]&/@{args}},
				"Arguments" -> {args},
				"FunctionDeclaration" -> decl
			|>]
		];

		argArr = If[Length[{args}] === 0, OpaqueRawPointer[0], RawMemoryExport[#["RawAST"]&/@{args}, "OpaqueRawPointer"]];

		Z3ASTObject[ctx, makeAppC[ctx["RawContext"], Information[decl, "RawFunctionDeclaration"], Length[{args}], argArr]]
	]

Z3FunctionDeclarationObject::appargcount = "Expected `1` arguments for function declaration `2`, but found `3` arguments instead: `4`."


(*
	Accessors
*)

getContext[Z3FunctionDeclarationObject[ctx_Z3ContextObject, rawDecl_]] := ctx
getRawDeclaration[Z3FunctionDeclarationObject[ctx_Z3ContextObject, rawDecl_]] := rawDecl


getFuncDeclDomainSizeC := getFuncDeclDomainSizeC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_domain_size", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

getDomainSize[decl_] := getFuncDeclDomainSizeC[getContext[decl]["RawContext"], getRawDeclaration[decl]]


getFuncDeclDomainC := getFuncDeclDomainC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_domain", {"OpaqueRawPointer", "OpaqueRawPointer", "CUnsignedInt"} -> "OpaqueRawPointer"];

getDomain[decl_] :=
	Enclose@Module[{ctx, rawDecl},
		ctx = getContext[decl];
		rawDecl = getRawDeclaration[decl];
		Z3SortObject[ctx, getFuncDeclDomainC[ctx["RawContext"], rawDecl, #-1]] &/@ Range[ConfirmMatch[getDomainSize[decl], _Integer]]
	]


getFuncDeclRangeC := getFuncDeclRangeC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_range", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

getRange[decl_] :=
	With[{ctx = getContext[decl]},
		Z3SortObject[ctx, getFuncDeclRangeC[ctx["RawContext"], getRawDeclaration[decl]]]
	]


(*
	Z3FunctionDeclarationObject
*)

Information`AddRegistry[Z3FunctionDeclarationObject, Z3FunctionDeclarationObjectInformation];

Z3FunctionDeclarationObjectInformation[decl: Z3FunctionDeclarationObject[ctx_Z3ContextObject, rawDecl_]] :=
	<|
		"ObjectType" -> "Z3FunctionDeclarationObject",
		"RawFunctionDeclaration" -> rawDecl,
		"Context" -> ctx,
		"ArgumentCount" :> getDomainSize[decl],
		"Domain" :> getDomain[decl],
		"Range" :> getRange[decl]
	|>

Z3FunctionDeclarationObject /: MakeBoxes[decl_Z3FunctionDeclarationObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3FunctionDeclarationObject,
		decl,
		None,
		{BoxForm`SummaryItem@{"raw function declaration: ", Information[decl, "RawFunctionDeclaration"]}},
		{},
		form
	]


End[];
EndPackage[];
