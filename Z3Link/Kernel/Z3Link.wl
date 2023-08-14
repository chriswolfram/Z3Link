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

ToZ3


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
Z3Equal
Z3Distinct
Z3Not
Z3If
Z3Equivalent
Z3Implies
Z3Xor
Z3And
Z3Or

(* Arithmetic *)
Z3Plus
Z3Times
Z3Subtract
Z3Minus
Z3Divide
Z3Mod
Z3Remainder
Z3Power
Z3Less
Z3LessEqual
Z3Greater
Z3GreaterEqual
Z3Divisible

(* Sets *)
Z3EmptySet
Z3FullSet

Z3SetInsert
Z3SetDelete
Z3Union
Z3Intersection
(* TODO: Complement vs ComplementSet is unclear. The problem is that the WL symbol is Complement, but there is no other good word for the operation. *)
Z3Complement
Z3ComplementSet
Z3Element
Z3Subset


Begin["`Private`"];


Needs["ChristopherWolfram`Z3Link`LibZ3`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`Simplify`"]
Needs["ChristopherWolfram`Z3Link`Symbol`"]
Needs["ChristopherWolfram`Z3Link`Sort`"]
Needs["ChristopherWolfram`Z3Link`FunctionDeclaration`"]
Needs["ChristopherWolfram`Z3Link`ASTVector`"]
Needs["ChristopherWolfram`Z3Link`Cast`"]
Needs["ChristopherWolfram`Z3Link`AST`"]
Needs["ChristopherWolfram`Z3Link`Solve`"]
Needs["ChristopherWolfram`Z3Link`Messages`"]


End[];
EndPackage[];
