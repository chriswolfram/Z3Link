BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Numerals`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeNumeralC := makeNumeralC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_numeral", {"OpaqueRawPointer", "RawPointer"::["CUnsignedChar"], "OpaqueRawPointer"} -> "OpaqueRawPointer"];

(*
	Z3Constant
*)

Options[Z3Numeral] = {Z3Context :> $Z3Context};

Z3Numeral[n_Integer, sortSpec_Z3SortObject, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3ASTObject[ctx, makeNumeralC[ctx["RawContext"], StringJoin[ToString/@IntegerDigits[n]], sortSpec["RawSort"]]]
	]

Z3Numeral[n_Integer, opts:OptionsPattern[]] :=
	Z3Numeral[n, CreateZ3Sort["Integer"], opts]


End[];
EndPackage[];
