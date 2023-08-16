BeginPackage["ChristopherWolfram`Z3Link`Sort`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


makeBooleanSortC := makeBooleanSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_bool_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeIntegerSortC := makeIntegerSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_int_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeRealSortC := makeRealSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_real_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeSetSortC := makeSetSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_set_sort", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeUninterpretedSortC := makeUninterpretedSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_uninterpreted_sort", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];


(*
	Z3SortCreate
*)

Options[Z3SortCreate] = {Z3Context :> $Z3Context};

Z3SortCreate[sortName_, opts:OptionsPattern[]] := iZ3SortCreate[OptionValue[Z3Context], sortName]

iZ3SortCreate[ctx_, "Boolean"] := Z3SortObject[ctx, makeBooleanSortC[ctx["RawContext"]]]
iZ3SortCreate[ctx_, "Integer"] := Z3SortObject[ctx, makeIntegerSortC[ctx["RawContext"]]]
iZ3SortCreate[ctx_, "Real"] := Z3SortObject[ctx, makeRealSortC[ctx["RawContext"]]]

iZ3SortCreate[ctx_, Booleans] := iZ3SortCreate[ctx, "Boolean"]
iZ3SortCreate[ctx_, Integers] := iZ3SortCreate[ctx, "Integer"]
iZ3SortCreate[ctx_, Reals] := iZ3SortCreate[ctx, "Real"]

iZ3SortCreate[ctx_, elem_Z3SortObject] :=
	Z3SortObject[ctx, makeSetSortC[ctx["RawContext"], elem["RawSort"]]]

iZ3SortCreate[ctx_, sym_Z3SymbolObject] :=
	Z3SortObject[ctx, makeUninterpretedSortC[ctx["RawContext"], sym["RawSymbol"]]]

iZ3SortCreate[ctx_, sortSpec_] :=
	If[MatchQ[ctx, _Z3ContextObject],
		Failure["InvalidSortSpecification", <|
			"MessageTemplate" :> "Invalid sort specification `1` encountered.",
			"MessageParameters" -> {sortSpec},
			"SortSpecification" -> sortSpec,
			"Context" -> ctx
		|>],
		Failure["InvalidZ3Context", <|
			"MessageTemplate" :> "Invalid Z3ContextObject `1` encountered.",
			"MessageParameters" -> {ctx},
			"Context" -> ctx
		|>]
	]


(*
	Z3SortObject
*)

Z3SortObject[args___] /; !argumentsZ3SortObject[args] :=
	With[{res = ArgumentsOptions[Z3SortObject[args], 2]},
		If[FailureQ[res],
			res,
			Message[Z3SortObject::inv, {args}];
			Failure["InvalidZ3SortObject", <|
				"MessageTemplate" :> Z3SortObject::inv,
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>]
		]
	]

argumentsZ3SortObject[_Z3ContextObject, _OpaqueRawPointer] := True
argumentsZ3SortObject[___] := False


(*
	Accessors
*)

HoldPattern[Z3SortObject][ctx_Z3ContextObject, rawSort_]["RawSort"] := rawSort
HoldPattern[Z3SortObject][ctx_Z3ContextObject, rawSort_]["Context"] := ctx


sortToASTC := sortToASTC =
	ForeignFunctionLoad[$LibZ3, "Z3_sort_to_ast", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

sort_Z3SortObject["AST"] := Z3ASTObject[sort["Context"], sortToASTC[sort["Context"]["RawContext"], sort["RawSort"]]]

sort_Z3SortObject["Hash"] := sort["AST"]["Hash"]


getSortNameC := getSortNameC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_sort_name", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

sort_Z3SortObject["Name"] := Z3SymbolObject[sort["Context"], getSortNameC[sort["Context"]["RawContext"], sort["RawSort"]]]


getSortKindC := getSortKindC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_sort_kind", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];

sort_Z3SortObject["KindID"] := getSortKindC[sort["Context"]["RawContext"], sort["RawSort"]]

$sortKindRawNames := $sortKindRawNames = AssociationMap[Reverse, $Z3ConstantsMap["SortKinds"]];

$sortKindCookedNames = <|
		"Z3_UNINTERPRETED_SORT" ->  "Uninterpreted",
		"Z3_BOOL_SORT" ->           "Boolean",
		"Z3_INT_SORT" ->            "Integer",
		"Z3_REAL_SORT" ->           "Real",
		"Z3_BV_SORT" ->             "BitVector",
		"Z3_ARRAY_SORT" ->          "Array",
		"Z3_DATATYPE_SORT" ->       "DataType",
		"Z3_RELATION_SORT" ->       "Relation",
		"Z3_FINITE_DOMAIN_SORT" ->  "FiniteDomain",
		"Z3_FLOATING_POINT_SORT" -> "FloatingPoint",
		"Z3_ROUNDING_MODE_SORT" ->  "RoundingMode",
		"Z3_SEQ_SORT" ->            "Sequence",
		"Z3_RE_SORT" ->             "RegularExpression",
		"Z3_CHAR_SORT" ->           "Character",
		"Z3_UNKNOWN_SORT" ->        "Unknown"
	|>;

sort_Z3SortObject["RawKind"] :=
	$sortKindRawNames[sort["KindID"]]

sort_Z3SortObject["Kind"] :=
	Replace[sort["RawKind"], $sortKindCookedNames]



Z3SortObject /: MakeBoxes[sort:Z3SortObject[args___] /; argumentsZ3SortObject[args], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3SortObject,
		sort,
		None,
		{BoxForm`SummaryItem@{"name: ", sort["Name"]}},
		{BoxForm`SummaryItem@{"raw sort: ", sort["RawSort"]}},
		form
	]


End[];
EndPackage[];
