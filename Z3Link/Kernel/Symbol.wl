BeginPackage["ChristopherWolfram`Z3Link`Symbol`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


makeIntegerSymbolC := makeIntegerSymbolC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_int_symbol", {"OpaqueRawPointer", "CInt"} -> "OpaqueRawPointer"];

makeStringSymbolC := makeStringSymbolC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_string_symbol", {"OpaqueRawPointer", "RawPointer"::["UnsignedInteger8"]} -> "OpaqueRawPointer"];

(*
	Z3SymbolCreate
*)

DeclareFunction[Z3SymbolCreate, iZ3SymbolCreate, 1];

Options[Z3SymbolCreate] = {Z3Context :> $Z3Context};

iZ3SymbolCreate[n_Integer, opts_] :=
	With[{ctx = OptionValue[Z3SymbolCreate, opts, Z3Context]},
		Z3SymbolObject[ctx, makeIntegerSymbolC[ctx["RawContext"], n]]
	]

iZ3SymbolCreate[str_?StringQ, opts_] :=
	With[{ctx = OptionValue[Z3SymbolCreate, opts, Z3Context]},
		Z3SymbolObject[ctx, makeStringSymbolC[ctx["RawContext"], RawMemoryExport[str]]]
	]


(*
	Z3SymbolObject
*)

DeclareObject[Z3SymbolObject, {_Z3ContextObject, _OpaqueRawPointer}];


(*
	Accessors
*)

sym_Z3SymbolObject["Context"] := sym[[1]]
sym_Z3SymbolObject["RawSymbol"] := sym[[2]]


getSymbolKindC := getSymbolKindC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_symbol_kind", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];
	
getSymbolIntC := getSymbolIntC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_symbol_int", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];
	
getSymbolStringC := getSymbolStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_symbol_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["CUnsignedChar"]];

$IDSymbolKinds := $IDSymbolKinds = AssociationMap[Reverse, $Z3ConstantsMap["SymbolKinds"]];

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

sym_Z3SymbolObject["Name"] := getSymbolName[sym["Context"]["RawContext"], sym["RawSymbol"]]


DeclareObjectFormatting[Z3SymbolObject,
	sym |-> {
		None,
		{
			{"name: ", sym["Name"]}
		},
		{
			{"raw symbol: ", sym["RawSymbol"]}
		}
	}]


End[];
EndPackage[];
