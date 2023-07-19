BeginPackage["ChristopherWolfram`Z3Link`Symbol`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


makeIntegerSymbolC := makeIntegerSymbolC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_int_symbol", {"OpaqueRawPointer", "CInt"} -> "OpaqueRawPointer"];

(*
	Z3SymbolCreate
*)

Options[Z3SymbolCreate] = {Z3Context :> $Z3Context};

Z3SymbolCreate[n_Integer, opts:OptionsPattern[]] :=
	With[{ctx = OptionValue[Z3Context]},
		Z3SymbolObject[ctx, makeIntegerSymbolC[ctx["RawContext"], n]]
	]


(*
	Z3SymbolObject
*)

getSymbolKindC := getSymbolKindC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_symbol_kind", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];
	
getSymbolIntC := getSymbolIntC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_symbol_int", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];
	
getSymbolStringC := getSymbolStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_symbol_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["CUnsignedChar"]];

$IDSymbolKinds := $IDSymbolKinds = AssociationMap[Reverse,$Z3ConstantsMap["SymbolKinds"]];

getSymbolName[ctx_, sym_] :=
	With[{kind = getSymbolKindC[ctx, sym]},
		Switch[kind,
		
			$Z3ConstantsMap["SymbolKinds"]["Z3_INT_SYMBOL"],
				getSymbolIntC[ctx, sym],
				
			$Z3ConstantsMap["SymbolKinds"]["Z3_STRING_SYMBOL"],
				RawMemoryImport[getSymbolStringC[ctx, sym], "String"],
			
			_,
				With[{kindName = Replace[kind, $IDSymbolKinds]},
					Message[Z3SymbolObject::unsupportedSymbolkind, kindName];
					Failure["UnsupportedKind", <|
						"MessageTemplate" :> Z3SymbolObject::unsupportedSymbolkind,
						"MessageParameters" -> {kindName},
						"SymbolKindName" -> kindName,
						"SymbolKindID" -> kind
					|>]
				]
		]
	]


Z3SymbolObject[ctx_, rawSym_]["RawSymbol"] := rawSym
Z3SymbolObject[ctx_, rawSym_]["Context"] := ctx
sym_Z3SymbolObject["Name"] := getSymbolName[sym["Context"]["RawContext"], sym["RawSymbol"]]

Z3SymbolObject /: MakeBoxes[sym_Z3SymbolObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3SymbolObject,
		sym,
		None,
		{BoxForm`SummaryItem@{"raw symbol: ", sym["RawSymbol"]}},
		{},
		form
	]


End[];
EndPackage[];
