BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Numerals`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"]


makeNumeralC := makeNumeralC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_numeral", {"OpaqueRawPointer", "RawPointer"::["CUnsignedChar"], "OpaqueRawPointer"} -> "OpaqueRawPointer"];

(*
	Z3Numeral
*)

Z3Numeral[n_Integer, sortSpec_Z3SortObject] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[sortSpec]},
		Z3ASTObject[ctx, makeNumeralC[ctx["RawContext"], integerString[n], sortSpec["RawSort"]]]
	]

Z3Numeral[n_Integer] :=
	Z3Numeral[n, Z3SortCreate["Integer"]]

Z3Numeral[n_Rational] :=
	With[{intSort = Z3SortCreate["Integer"]},
		Z3Divide[
			Z3Numeral[Numerator[n], intSort],
			Z3Numeral[Denominator[n], intSort]
		]
	]


integerString[n_] :=
	If[Negative[n],"-",""]<>StringJoin[ToString/@IntegerDigits[n]]


End[];
EndPackage[];
