BeginPackage["ChristopherWolfram`Z3Link`AST`Z3ASTObject`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


(*
	Verifiers
*)

Z3ASTObject[args___] /; !argumentsZ3ASTObject[args] :=
	With[{res = ArgumentsOptions[Z3ASTObject[args], 2]},
		If[FailureQ[res],
			res,
			Message[Z3ASTObject::inv, {args}];
			Failure["InvalidZ3ASTObject", <|
				"MessageTemplate" :> Z3ASTObject::inv,
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>]
		]
	]

argumentsZ3ASTObject[_Z3ContextObject, _OpaqueRawPointer] := True
argumentsZ3ASTObject[___] := False


(*
	Accessors
*)

astToStringC := astToStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_to_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["CUnsignedChar"]];

astToString[ctx_, ast_] :=
	RawMemoryImport[astToStringC[ctx, ast], "String"]


getASTKindC := getASTKindC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_ast_kind", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];
	
getASTAppC := getASTAppC =
	ForeignFunctionLoad[$LibZ3, "Z3_to_app", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

unwrapAST[ctx_, ast_]:=
	With[{kind = getASTKindC[ctx,ast]},
		Switch[kind,
		
			$Z3ConstantsMap["ASTKinds"]["Z3_APP_AST"],
				(* TODO HERE *)
				fromApp[ctx, getASTAppC[ctx, ast]],
		
			_,
				With[{kindName = Replace[kind, AssociationMap[Reverse,$ASTKindIDs]]},
					Message[Z3ASTObject::unsupportedASTkind, kindName];
					Failure["UnsupportedKind", <|
						"MessageTemplate" :> Z3ASTObject::unsupportedASTkind,
						"MessageParameters" -> {kindName},
						"ASTKindName" -> kindName,
						"ASTKindID" -> kind
					|>]
				]
		]
	]

HoldPattern[Z3ASTObject][ctx_Z3ContextObject, rawAST_]["Context"] := ctx
HoldPattern[Z3ASTObject][ctx_Z3ContextObject, rawAST_]["RawAST"] := rawAST

ast_Z3ASTObject["Unwrap"] := unwrapAST[ast["Context"]["RawContext"], ast["RawAST"]]
ast_Z3ASTObject["String"] := astToString[ast["Context"]["RawContext"], ast["RawAST"]]


getASTSortC := getASTSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_sort", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

ast_Z3ASTObject["Sort"] := Z3SortObject[ast["Context"], getASTSortC[ast["Context"]["RawContext"], ast["RawAST"]]]


getASTHashC := getASTHashC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_ast_hash", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

ast_Z3ASTObject["Hash"] := getASTHashC[ast["Context"]["RawContext"], ast["RawAST"]]


(*
	Summary box
*)

Z3ASTObject /: MakeBoxes[ast:Z3ASTObject[args___] /; argumentsZ3ASTObject[args], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3ASTObject,
		ast,
		None,
		{BoxForm`SummaryItem@{"", ast["String"]}},
		{
			BoxForm`SummaryItem@{"raw AST: ", ast["RawAST"]},
			BoxForm`SummaryItem@{"context: ", ast["Context"]}
		},
		form
	]


End[];
EndPackage[];
