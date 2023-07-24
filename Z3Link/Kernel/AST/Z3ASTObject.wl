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

HoldPattern[Z3ASTObject][ctx_Z3ContextObject, rawAST_]["Context"] := ctx
HoldPattern[Z3ASTObject][ctx_Z3ContextObject, rawAST_]["RawAST"] := rawAST

(* String *)

astToStringC := astToStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_to_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["CUnsignedChar"]];

astToString[ctx_, ast_] :=
	RawMemoryImport[astToStringC[ctx, ast], "String"]

ast_Z3ASTObject["String"] := astToString[ast["Context"]["RawContext"], ast["RawAST"]]


(* Sort *)

getASTSortC := getASTSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_sort", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

ast_Z3ASTObject["Sort"] := Z3SortObject[ast["Context"], getASTSortC[ast["Context"]["RawContext"], ast["RawAST"]]]


(* Hash *)

getASTHashC := getASTHashC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_ast_hash", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

ast_Z3ASTObject["Hash"] := getASTHashC[ast["Context"]["RawContext"], ast["RawAST"]]


(* RawKind *)

getASTKindC := getASTKindC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_ast_kind", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];

ast_Z3ASTObject["RawKind"] :=
	getASTKindC[ast["Context"]["RawContext"], ast["RawAST"]]


(* Kind *)

$astKindRawNames := $astKindRawNames = AssociationMap[Reverse, $Z3ConstantsMap["ASTKinds"]];

$astKindCookedNames = <|
	"Z3_NUMERAL_AST" ->    "Numeral",
	"Z3_APP_AST" ->        "Application",
	"Z3_VAR_AST" ->        "BoundVariable",
	"Z3_QUANTIFIER_AST" -> "Quantifier",
	"Z3_SORT_AST" ->       "Sort",
	"Z3_FUNC_DECL_AST" ->  "FunctionDeclaration",
	"Z3_UNKNOWN_AST" ->    "Internal"
|>;

ast_Z3ASTObject["Kind"] :=
	Replace[$astKindRawNames[ast["RawKind"]], $astKindCookedNames]


(* ArgumentCount *)

getAppNumArgsC := getAppNumArgsC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_app_num_args", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

ast_Z3ASTObject["ArgumentCount"] := 
	Enclose@Module[{app},
		app = Confirm@getRawApplication[ast];
		getAppNumArgsC[ast["Context"]["RawContext"], app]
	]


(* Arguments *)

getAppArgC := getAppArgC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_app_arg", {"OpaqueRawPointer", "OpaqueRawPointer", "CUnsignedInt"} -> "OpaqueRawPointer"];

ast_Z3ASTObject["Arguments"] := 
	Enclose@Module[{app, argCount},
		app = Confirm@getRawApplication[ast];
		argCount = getAppNumArgsC[ast["Context"]["RawContext"], app];
		Z3ASTObject[ast["Context"], getAppArgC[ast["Context"]["RawContext"], app, #-1]]&/@Range[argCount]
	]

ast_Z3ASTObject["Arguments", i_Integer?Positive] := 
	Enclose@Module[{app, argCount},
		app = Confirm@getRawApplication[ast];
		argCount = getAppNumArgsC[ast["Context"]["RawContext"], app];
		If[i > argCount,
			Message[Z3ASTObject::arg, i, ast];
			Confirm@Failure["InvalidArgumentIndex", <|
				"MessageTemplate" :> Z3ASTObject::arg,
				"MessageParameters" -> {i, ast},
				"AST" -> ast,
				"ArgumentCount" -> argCount,
				"Index" -> i
			|>]
		];
		Z3ASTObject[ast["Context"], getAppArgC[ast["Context"]["RawContext"], app, i-1]]
	]


(* Head *)

getAppDeclC := getAppDeclC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_app_decl", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

ast_Z3ASTObject["Head"] := 
	Enclose@Module[{app},
		app = Confirm@getRawApplication[ast];
		Z3FunctionDeclarationObject[ast["Context"], getAppDeclC[ast["Context"]["RawContext"], app]]
	]


(* Errors *)

getASTAppC := getASTAppC =
	ForeignFunctionLoad[$LibZ3, "Z3_to_app", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

getRawApplication[ast_] :=
	If[ast["RawKind"] === $Z3ConstantsMap["ASTKinds"]["Z3_APP_AST"],
		getASTAppC[ast["Context"]["RawContext"], ast["RawAST"]],
		Message[Z3ASTObject::app, ast["Kind"], ast];
		Failure["InvalidApplicationAST", <|
			"MessageTemplate" :> Z3ASTObject::app,
			"MessageParameters" -> {ast["Kind"], ast},
			"AST" -> ast
		|>]
	]


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
