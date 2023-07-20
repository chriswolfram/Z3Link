BeginPackage["ChristopherWolfram`Z3Link`Messages`"];

Needs["ChristopherWolfram`Z3Link`"]


Z3ASTObject::unsupportedASTkind = "Unsupported AST kind `1`.";

Z3SymbolObject::unsupportedSymbolkind = "Unsupported symbol kind `1`.";

(* TODO: Use ArgumentsOptions? *)
Z3FunctionDeclarationObject::appargcount = "Expected `1` arguments for function declaration `2`, but found `3` arguments instead: `4`.";
Z3FunctionDeclarationObject::appargsort = "Expected arguments sorts `1` for function declaration `2`, but found `3` instead.";


EndPackage[];
