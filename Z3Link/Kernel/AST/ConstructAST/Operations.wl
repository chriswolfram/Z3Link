BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Operations`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


(* Arithmetic *)

DefineASTConstructor[Z3Plus, "Z3_mk_add", {1,Infinity}];
DefineASTConstructor[Z3Subtract, "Z3_mk_sub", {1,Infinity}];
DefineASTConstructor[Z3Times, "Z3_mk_mul", {1,Infinity}];
DefineASTConstructor[Z3Minus, "Z3_mk_unary_minus", 1];
DefineASTConstructor[Z3Divide, "Z3_mk_div", {"Real", "Real"}];
DefineASTConstructor[Z3Quotient, "Z3_mk_div", {"Integer", "Integer"}];
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
DefineASTConstructor[Z3Equal, "Z3_mk_eq", 2, #1["Sort"]["Hash"] === #2["Sort"]["Hash"]&];
DefineASTConstructor[Z3Distinct, "Z3_mk_distinct", {1,Infinity}];
DefineASTConstructor[Z3Not, "Z3_mk_not", {"Boolean"}];
DefineASTConstructor[Z3If, "Z3_mk_ite", {"Boolean", Automatic, Automatic}, #2["Sort"]["Hash"] === #3["Sort"]["Hash"]&];
DefineASTConstructor[Z3Equivalent, "Z3_mk_iff", 2];
DefineASTConstructor[Z3Implies, "Z3_mk_implies", {"Boolean", "Boolean"}];
DefineASTConstructor[Z3Xor, "Z3_mk_xor", {"Boolean", "Boolean"}];
DefineASTConstructor[Z3And, "Z3_mk_and", {1,Infinity}];
DefineASTConstructor[Z3Or, "Z3_mk_or", {1,Infinity}];


(* Sets *)

DefineASTConstructor[Z3SetInsert, "Z3_mk_set_add", 2];
DefineASTConstructor[Z3SetDelete, "Z3_mk_set_del", 2];
DefineASTConstructor[Z3Union, "Z3_mk_set_union", {1,Infinity}];
DefineASTConstructor[Z3Intersection, "Z3_mk_set_intersect", {1,Infinity}];
DefineASTConstructor[Z3Complement, "Z3_mk_set_difference", 2];
DefineASTConstructor[Z3ComplementSet, "Z3_mk_set_complement", 1];
DefineASTConstructor[Z3Element, "Z3_mk_set_member", 2];
DefineASTConstructor[Z3Subset, "Z3_mk_set_subset", 2];


(* Other *)

DefineASTConstructor[Z3IntegerToReal, "Z3_mk_int2real", {"Integer"}];


End[];
EndPackage[];
