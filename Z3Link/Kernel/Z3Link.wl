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

Z3ApplicationCreate
Z3ApplicationObject

Z3Simplify


(* Specific AST constructors *)

(* Basic *)
Z3Constant
Z3Numeral

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
Needs["ChristopherWolfram`Z3Link`Application`"]
Needs["ChristopherWolfram`Z3Link`FunctionDeclaration`"]
Needs["ChristopherWolfram`Z3Link`AST`"]
Needs["ChristopherWolfram`Z3Link`Messages`"]


End[];
EndPackage[];
