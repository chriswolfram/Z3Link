BeginPackage["ChristopherWolfram`Z3Link`Symbol`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeIntegerSymbolC := makeIntegerSymbolC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_int_symbol", {"OpaqueRawPointer", "CInt"} -> "OpaqueRawPointer"];

(*
	CreateZ3Symbol
*)

Options[CreateZ3Symbol] = {Z3Context :> $Z3Context};

CreateZ3Symbol[n_Integer, opts:OptionsPattern[]] :=
	Z3SymbolObject[makeIntegerSymbolC[OptionValue[Z3Context]["RawContext"], n]]


(*
	Z3SymbolObject
*)

Z3SymbolObject[rawSym_]["RawSymbol"] := rawSym

Z3SymbolObject /: MakeBoxes[sym_Z3SymbolObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3SymbolObject,
		sym,
		None,
		{"raw symbol: ", sym["RawSymbol"]},
		{},
		form
	]


End[];
EndPackage[];
