BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`UpValues`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


(* Arithmetic *)

Z3ASTObject /: Plus[ast_Z3ASTObject, rest___] := Z3Plus[ast, rest]

Z3ASTObject /: Subtract[lhs_Z3ASTObject, rhs_] := Z3Subtract[lhs, rhs]
Z3ASTObject /: Subtract[lhs_, rhs_Z3ASTObject] := Z3Subtract[lhs, rhs]

Z3ASTObject /: Times[ast_Z3ASTObject, rest___] := Z3Times[ast, rest]

Z3ASTObject /: Minus[ast_Z3ASTObject] := Z3Minus[ast]

(* TODO: Unclear if these actually work. Divide seems to run too eagerly. *)
Z3ASTObject /: Divide[lhs_Z3ASTObject, rhs_] := Z3Divide[lhs, rhs]
Z3ASTObject /: Divide[lhs_, rhs_Z3ASTObject] := Z3Divide[lhs, rhs]

Z3ASTObject /: Mod[lhs_Z3ASTObject, rhs_] := Z3Mod[lhs, rhs]
Z3ASTObject /: Mod[lhs_, rhs_Z3ASTObject] := Z3Mod[lhs, rhs]

Z3ASTObject /: Power[lhs_Z3ASTObject, rhs_] := Z3Power[lhs, rhs]
Z3ASTObject /: Power[lhs_, rhs_Z3ASTObject] := Z3Power[lhs, rhs]

Z3ASTObject /: Less[lhs_Z3ASTObject, rhs_] := Z3Less[lhs, rhs]
Z3ASTObject /: Less[lhs_, rhs_Z3ASTObject] := Z3Less[lhs, rhs]

Z3ASTObject /: LessEqual[lhs_Z3ASTObject, rhs_] := Z3LessEqual[lhs, rhs]
Z3ASTObject /: LessEqual[lhs_, rhs_Z3ASTObject] := Z3LessEqual[lhs, rhs]

Z3ASTObject /: Greater[lhs_Z3ASTObject, rhs_] := Z3Greater[lhs, rhs]
Z3ASTObject /: Greater[lhs_, rhs_Z3ASTObject] := Z3Greater[lhs, rhs]

Z3ASTObject /: GreaterEqual[lhs_Z3ASTObject, rhs_] := Z3GreaterEqual[lhs, rhs]
Z3ASTObject /: GreaterEqual[lhs_, rhs_Z3ASTObject] := Z3GreaterEqual[lhs, rhs]

Z3ASTObject /: Divisible[lhs_Z3ASTObject, rhs_] := Z3Divisible[lhs, rhs]
Z3ASTObject /: Divisible[lhs_, rhs_Z3ASTObject] := Z3Divisible[lhs, rhs]


(* Logic *)

Z3ASTObject /: Equal[lhs_Z3ASTObject, rhs_] := Z3Equal[lhs, rhs]
Z3ASTObject /: Equal[lhs_, rhs_Z3ASTObject] := Z3Equal[lhs, rhs]

Z3ASTObject /: Not[ast_Z3ASTObject] := Z3Not[ast]

(* This breaks a lot of things: *)
(* Z3ASTObject /: If[a_Z3ASTObject, b_, c_] := Z3If[a, b, c]
Z3ASTObject /: If[a_, b_Z3ASTObject, c_] := Z3If[a, b, c]
Z3ASTObject /: If[a_, b_, c_Z3ASTObject] := Z3If[a, b, c] *)

Z3ASTObject /: Equivalent[lhs_Z3ASTObject, rhs_] := Z3Equivalent[lhs, rhs]
Z3ASTObject /: Equivalent[lhs_, rhs_Z3ASTObject] := Z3Equivalent[lhs, rhs]

Z3ASTObject /: Implies[lhs_Z3ASTObject, rhs_] := Z3Implies[lhs, rhs]
Z3ASTObject /: Implies[lhs_, rhs_Z3ASTObject] := Z3Implies[lhs, rhs]

Z3ASTObject /: Xor[lhs_Z3ASTObject, rhs_] := Z3Implies[lhs, rhs]

Z3ASTObject /: And[ast_Z3ASTObject, rest___] := Z3And[ast, rest]

Z3ASTObject /: Or[ast_Z3ASTObject, rest___] := Z3And[ast, rest]


End[];
EndPackage[];
