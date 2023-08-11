BeginPackage["ChristopherWolfram`Z3Link`"];

$LibZ3

Z3ContextObject
Z3ContextCreate
Z3Context
$Z3Context

Z3ASTObject

Z3SymbolCreate
Z3SymbolObject

Z3SortCreate
Z3SortObject

Z3FunctionDeclarationCreate
Z3FunctionDeclarationObject

Z3Simplify

Z3SolverCreate
Z3SolverObject

Z3ModelObject


(* Specific AST constructors *)

(* Basic *)
Z3Constant
Z3Numeral

(* Quantifiers *)
Z3ForAll
Z3Exists
Z3DeBruijnIndex

(* Logic *)
Z3True
Z3False
Z3Not
Z3Equal

(* Arithmetic *)
Z3Plus
Z3Times


Begin["`Private`"];


Needs["ChristopherWolfram`Z3Link`LibZ3`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`Simplify`"]
Needs["ChristopherWolfram`Z3Link`Symbol`"]
Needs["ChristopherWolfram`Z3Link`Sort`"]
Needs["ChristopherWolfram`Z3Link`FunctionDeclaration`"]
Needs["ChristopherWolfram`Z3Link`ASTVector`"]
Needs["ChristopherWolfram`Z3Link`AST`"]
Needs["ChristopherWolfram`Z3Link`Solve`"]
Needs["ChristopherWolfram`Z3Link`Messages`"]


End[];
EndPackage[];
