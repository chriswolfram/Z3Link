BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Operations`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


(* Arithmetic *)

DeclareASTConstructor[Z3Plus, "Z3_mk_add", {1,Infinity}];
DeclareASTConstructor[Z3Subtract, "Z3_mk_sub", {1,Infinity}];
DeclareASTConstructor[Z3Times, "Z3_mk_mul", {1,Infinity}];
DeclareASTConstructor[Z3Minus, "Z3_mk_unary_minus", 1];
DeclareASTConstructor[Z3Divide, "Z3_mk_div", 2, {"Real", "Real"}];
DeclareASTConstructor[Z3Quotient, "Z3_mk_div", 2, {"Integer", "Integer"}];
DeclareASTConstructor[Z3Mod, "Z3_mk_mod", 2];
DeclareASTConstructor[Z3Remainder, "Z3_mk_rem", 2];
DeclareASTConstructor[Z3Power, "Z3_mk_power", 2];
DeclareASTConstructor[Z3Less, "Z3_mk_lt", 2];
DeclareASTConstructor[Z3LessEqual, "Z3_mk_le", 2];
DeclareASTConstructor[Z3Greater, "Z3_mk_gt", 2];
DeclareASTConstructor[Z3GreaterEqual, "Z3_mk_ge", 2];
DeclareASTConstructor[Z3Divisible, "Z3_mk_divides", 2];


(* Logic *)

DeclareASTConstructor[Z3True, "Z3_mk_true", 0];
DeclareASTConstructor[Z3False, "Z3_mk_false", 0];
DeclareASTConstructor[Z3Equal, "Z3_mk_eq", 2, #1["Sort"]["Hash"] === #2["Sort"]["Hash"]&];
DeclareASTConstructor[Z3Distinct, "Z3_mk_distinct", {1,Infinity}];
DeclareASTConstructor[Z3Not, "Z3_mk_not", 1, {"Boolean"}];
DeclareASTConstructor[Z3If, "Z3_mk_ite", 3, {"Boolean", Automatic, Automatic}, #2["Sort"]["Hash"] === #3["Sort"]["Hash"]&];
DeclareASTConstructor[Z3Equivalent, "Z3_mk_iff", 2];
DeclareASTConstructor[Z3Implies, "Z3_mk_implies", 2, {"Boolean", "Boolean"}];
DeclareASTConstructor[Z3Xor, "Z3_mk_xor", 2, {"Boolean", "Boolean"}];
DeclareASTConstructor[Z3And, "Z3_mk_and", {1,Infinity}, "Boolean"];
DeclareASTConstructor[Z3Or, "Z3_mk_or", {1,Infinity}, "Boolean"];


(* Sets *)

DeclareASTConstructor[Z3SetInsert, "Z3_mk_set_add", 2];
DeclareASTConstructor[Z3SetDelete, "Z3_mk_set_del", 2];
DeclareASTConstructor[Z3Union, "Z3_mk_set_union", {1,Infinity}];
DeclareASTConstructor[Z3Intersection, "Z3_mk_set_intersect", {1,Infinity}];
DeclareASTConstructor[Z3Complement, "Z3_mk_set_difference", 2];
DeclareASTConstructor[Z3ComplementSet, "Z3_mk_set_complement", 1];
DeclareASTConstructor[Z3Element, "Z3_mk_set_member", 2];
DeclareASTConstructor[Z3Subset, "Z3_mk_set_subset", 2];


(* Other *)

DeclareASTConstructor[Z3IntegerToReal, "Z3_mk_int2real", 1, {"Integer"}];


End[];
EndPackage[];
