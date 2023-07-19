BeginPackage["ChristopherWolfram`Z3Link`AST`Z3ASTObject`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


astToStringC := astToStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_to_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["CUnsignedChar"]];

astToString[ctx_, ast_] :=
	RawMemoryImport[astToStringC[ctx, ast], "String"]


(*
	Z3ASTObject
*)

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


Z3ASTObject[ctx_Z3ContextObject, rawAST_]["Context"] := ctx
Z3ASTObject[ctx_Z3ContextObject, rawAST_]["RawAST"] := rawAST

ast_Z3ASTObject["Unwrap"] := unwrapAST[ast["Context"]["RawContext"], ast["RawAST"]]
ast_Z3ASTObject["String"] := astToString[ast["Context"]["RawContext"], ast["RawAST"]]

Z3ASTObject /: MakeBoxes[ast_Z3ASTObject, form:StandardForm]:=
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
