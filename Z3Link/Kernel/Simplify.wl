BeginPackage["ChristopherWolfram`Z3Link`Simplify`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


(*
	Z3Simplify
*)

simplifyC := simplifyC = 
	ForeignFunctionLoad[$LibZ3, "Z3_simplify", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

Z3Simplify[ast_Z3ASTObject] :=
	Z3ASTObject[ast["Context"], simplifyC[ast["Context"]["RawContext"], ast["RawAST"]]]


End[];
EndPackage[];
