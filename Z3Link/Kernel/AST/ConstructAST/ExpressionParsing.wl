BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`ExpressionParsing`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


(* Arithmetic *)

DefineExpressionParsing[Plus, Z3Plus];
DefineExpressionParsing[Subtract, Z3Subtract];
DefineExpressionParsing[Times, Z3Times];
DefineExpressionParsing[Minus, Z3Minus];
DefineExpressionParsing[Divide, Z3Divide];
DefineExpressionParsing[Quotient, Z3Quotient];
DefineExpressionParsing[Mod, Z3Mod];
DefineExpressionParsing[Power, Z3Power];
DefineExpressionParsing[Less, Z3Less];
DefineExpressionParsing[LessEqual, Z3LessEqual];
DefineExpressionParsing[Greater, Z3Greater];
DefineExpressionParsing[GreaterEqual, Z3GreaterEqual];
DefineExpressionParsing[Divisible, Z3Divisible];


(* Logic *)

DefineExpressionParsing[Equal, Z3Equal];
DefineExpressionParsing[Not, Z3Not];
DefineExpressionParsing[If, Z3If];
DefineExpressionParsing[Equivalent, Z3Equivalent];
DefineExpressionParsing[Implies, Z3Implies];
DefineExpressionParsing[Xor, Z3Xor];
DefineExpressionParsing[And, Z3And];
DefineExpressionParsing[Or, Z3Or];

(* Quantifiers *)

DefineExpressionParsing[ForAll, Z3ForAll];
DefineExpressionParsing[Exists, Z3Exists];


End[];
EndPackage[];
