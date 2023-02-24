BeginPackage["ChristopherWolfram`Z3Link`AST`Z3ASTObject`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


astToStringC := astToStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_to_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["CUnsignedChar"]];

astToString[ctx_, ast_] :=
	ImportRawMemory[astToStringC[ctx, ast], "String"]


(*
	Z3ASTObject
*)

Z3ASTObject[ctx_Z3ContextObject, rawAST_]["Context"] := ctx
Z3ASTObject[ctx_Z3ContextObject, rawAST_]["RawAST"] := rawAST

ast_Z3ASTObject["String"] := astToString[ast["Context"]["RawContext"], ast["RawAST"]]

Z3ASTObject /: MakeBoxes[ast_Z3ASTObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3ASTObject,
		ast,
		None,
		{"", ast["String"]},
		{
			{"raw AST: ", ast["RawAST"]},
			{"context: ", ast["Context"]}
		},
		form
	]


End[];
EndPackage[];
