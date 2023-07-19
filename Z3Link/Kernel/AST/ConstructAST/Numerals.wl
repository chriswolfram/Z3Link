BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Numerals`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


makeNumeralC := makeNumeralC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_numeral", {"OpaqueRawPointer", "RawPointer"::["CUnsignedChar"], "OpaqueRawPointer"} -> "OpaqueRawPointer"];

(*
	Z3Constant
*)

Z3Numeral[n_Integer, sortSpec_Z3SortObject, opts:OptionsPattern[]] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[sortSpec]},
		Z3ASTObject[ctx, makeNumeralC[ctx["RawContext"], StringJoin[ToString/@IntegerDigits[n]], sortSpec["RawSort"]]]
	]

Z3Numeral[n_Integer, opts:OptionsPattern[]] :=
	Z3Numeral[n, CreateZ3Sort["Integer"], opts]


End[];
EndPackage[];
