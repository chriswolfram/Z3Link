BeginPackage["ChristopherWolfram`Z3Link`Messages`"];

Needs["ChristopherWolfram`Z3Link`"]

Z3ContextObject::inv = "Invalid arguments for Z3ContextObject `1`.";

Z3ASTObject::inv = "Invalid arguments for Z3ASTObject `1`.";
Z3ASTObject::unsupportedASTkind = "Unsupported AST kind `1`.";
Z3ASTObject::kind = "Expected a Z3ASTObject with kind `1`, but found one with kind `2` instead: `3`.";
Z3ASTObject::arg = "Argument `1` of `2` does not exist.";

Z3FunctionDeclarationObject::inv = "Invalid arguments for Z3FunctionDeclarationObject `1`.";

Z3SortObject::inv = "Invalid arguments for Z3SortObject `1`.";

Z3SymbolObject::inv = "Invalid arguments for Z3SymbolObject `1`.";
Z3SymbolObject::unsupportedSymbolkind = "Unsupported symbol kind `1`.";

(* TODO: Use ArgumentsOptions? *)
Z3FunctionDeclarationObject::appargcount = "Expected `1` arguments for function declaration `2`, but found `3` arguments instead: `4`.";
Z3FunctionDeclarationObject::appargsort = "Expected arguments sorts `1` for function declaration `2`, but found `3` instead.";

Z3SolverObject::inv = "Invalid arguments for Z3SolverObject `1`.";


Needs["ChristopherWolfram`Z3Link`ASTVector`"]

Z3ASTVectorObject::inv = "Invalid arguments for Z3ASTVectorObject `1`.";


EndPackage[];
