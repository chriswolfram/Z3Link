BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Logic`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeTrueC := makeTrueC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_true", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeFalseC := makeFalseC = 
	ForeignFunctionLoad[$LibZ3, "Z3_mk_false", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeNotC := makeNotC = 
	ForeignFunctionLoad[$LibZ3, "Z3_mk_not", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeEqualC := makeEqualsC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_eq", {"OpaqueRawPointer", "OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];


(*
	Z3True
*)

Options[Z3True] = {Z3Context :> $Z3Context};

Z3True[opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeTrueC[ctx["RawContext"]]]
	]


(*
	Z3False
*)

Options[Z3True] = {Z3Context :> $Z3Context};

Z3False[opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeFalseC[ctx["RawContext"]]]
	]


(*
	Z3Not
*)

Options[Z3Not] = {Z3Context :> $Z3Context};

Z3Not[ast_Z3ASTObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeNotC[ctx["RawContext"], ast["RawAST"]]]
	]


(*
	Z3Equal
*)

Options[Z3Equal] = {Z3Context :> $Z3Context};

Z3Equal[lhs_Z3ASTObject, rhs_Z3ASTObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeEqualC[ctx["RawContext"], lhs["RawAST"], rhs["RawAST"]]]
	]


End[];
EndPackage[];
