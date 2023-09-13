BeginPackage["ChristopherWolfram`Z3Link`Simplify`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]


(*
	Z3Simplify
*)

simplifyC := simplifyC = 
	ForeignFunctionLoad[$LibZ3, "Z3_simplify", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

DeclareFunction[Z3Simplify, iZ3Simplify, 1];

iZ3Simplify[ast_Z3ASTObject, opts_] :=
	Z3ASTObject[ast["Context"], simplifyC[ast["Context"]["RawContext"], ast["RawAST"]]]


End[];
EndPackage[];
