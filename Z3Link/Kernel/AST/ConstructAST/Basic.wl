BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Basic`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


(* Arithmetic *)

DefineASTConstructor[Z3Plus, "Z3_mk_add", {1,Infinity}];
DefineASTConstructor[Z3Subtract, "Z3_mk_sub", {1,Infinity}];
DefineASTConstructor[Z3Times, "Z3_mk_mul", {1,Infinity}];
DefineASTConstructor[Z3Minus, "Z3_mk_unary_minus", 1];
DefineASTConstructor[Z3Divide, "Z3_mk_div", 2];
DefineASTConstructor[Z3Mod, "Z3_mk_mod", 2];
DefineASTConstructor[Z3Remainder, "Z3_mk_rem", 2];
DefineASTConstructor[Z3Power, "Z3_mk_power", 2];
DefineASTConstructor[Z3Less, "Z3_mk_lt", 2];
DefineASTConstructor[Z3LessEqual, "Z3_mk_le", 2];
DefineASTConstructor[Z3Greater, "Z3_mk_gt", 2];
DefineASTConstructor[Z3GreaterEqual, "Z3_mk_ge", 2];
DefineASTConstructor[Z3Divisible, "Z3_mk_divides", 2];


(* Logic *)

DefineASTConstructor[Z3True, "Z3_mk_true", 0];
DefineASTConstructor[Z3False, "Z3_mk_false", 0];
DefineASTConstructor[Z3Equal, "Z3_mk_eq", 2];
DefineASTConstructor[Z3Distinct, "Z3_mk_distinct", {1,Infinity}];
DefineASTConstructor[Z3Not, "Z3_mk_not", 1];
DefineASTConstructor[Z3If, "Z3_mk_ite", 3];
DefineASTConstructor[Z3Equivalent, "Z3_mk_iff", 2];
DefineASTConstructor[Z3Implies, "Z3_mk_implies", 2];
DefineASTConstructor[Z3Xor, "Z3_mk_xor", 2];
DefineASTConstructor[Z3And, "Z3_mk_and", {1,Infinity}];
DefineASTConstructor[Z3Or, "Z3_mk_or", {1,Infinity}];


(* Sets *)

DefineASTConstructor[Z3True, "Z3_mk_true", 0];
DefineASTConstructor[Z3False, "Z3_mk_false", 0];
DefineASTConstructor[Z3Equal, "Z3_mk_eq", 2];
DefineASTConstructor[Z3Distinct, "Z3_mk_distinct", {1,Infinity}];
DefineASTConstructor[Z3Not, "Z3_mk_not", 1];
DefineASTConstructor[Z3If, "Z3_mk_ite", 3];
DefineASTConstructor[Z3Equivalent, "Z3_mk_iff", 2];
DefineASTConstructor[Z3Implies, "Z3_mk_implies", 2];
DefineASTConstructor[Z3Xor, "Z3_mk_xor", 2];
DefineASTConstructor[Z3And, "Z3_mk_and", {1,Infinity}];
DefineASTConstructor[Z3Or, "Z3_mk_or", {1,Infinity}];


End[];
EndPackage[];
