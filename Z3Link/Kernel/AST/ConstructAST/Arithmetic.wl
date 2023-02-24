BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Arithmetic`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeAddC := makeAddC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_add", {"OpaqueRawPointer", "CUnsignedInt", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"];

makeMultiplyC := makeMultiplyC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_mul", {"OpaqueRawPointer", "CUnsignedInt", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"];

(*
	Z3Plus
*)

Options[Z3Plus] = {Z3Context :> $Z3Context};

Z3Plus[args___Z3ASTObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
	Module[{argArray = ExportRawMemory[#["RawAST"] &/@ {args}, "RawPointer"::["OpaqueRawPointer"]]},
		Z3ASTObject[ctx, makeAddC[ctx["RawContext"], Length[{args}], argArray]]
	]]


(*
	Z3Times
*)

Options[Z3Times] = {Z3Context :> $Z3Context};

Z3Times[args___Z3ASTObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
	Module[{argArray = ExportRawMemory[#["RawAST"] &/@ {args}, "RawPointer"::["OpaqueRawPointer"]]},
		Z3ASTObject[ctx, makeMultiplyC[ctx["RawContext"], Length[{args}], argArray]]
	]]


End[];
EndPackage[];
