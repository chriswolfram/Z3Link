BeginPackage["ChristopherWolfram`Z3Link`Sort`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


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

(* TODO: Add fallthroughs for both arguments*)
iZ3SortCreate[ctx_, "Boolean"] := Z3SortObject[ctx, makeBooleanSortC[ctx["RawContext"]]]
iZ3SortCreate[ctx_, "Integer"] := Z3SortObject[ctx, makeIntegerSortC[ctx["RawContext"]]]
iZ3SortCreate[ctx_, "Real"] := Z3SortObject[ctx, makeRealSortC[ctx["RawContext"]]]

iZ3SortCreate[ctx_, elem_Z3SortObject] :=
	Z3SortObject[ctx, makeSetSortC[ctx["RawContext"], elem["RawSort"]]]

iZ3SortCreate[ctx_, sym_Z3SymbolObject] :=
	Z3SortObject[ctx, makeUninterpretedSortC[ctx["RawContext"], sym["RawSymbol"]]]


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


Z3SortObject /: MakeBoxes[sort:Z3SortObject[args___] /; argumentsZ3SortObject[args], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3SortObject,
		sort,
		None,
		{BoxForm`SummaryItem@{"raw sort: ", sort["RawSort"]}},
		{},
		form
	]


End[];
EndPackage[];
