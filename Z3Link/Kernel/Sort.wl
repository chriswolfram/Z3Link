BeginPackage["ChristopherWolfram`Z3Link`Sort`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


makeBooleanSortC := makeBooleanSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_bool_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeIntegerSortC := makeIntegerSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_int_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeRealSortC := makeRealSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_real_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];


(*
	Z3SortCreate
*)

Options[Z3SortCreate] = {Z3Context :> $Z3Context};

Z3SortCreate[sortName_, opts:OptionsPattern[]] := iZ3SortCreate[OptionValue[Z3Context], sortName]

(* TODO: Add fallthrough *)
iZ3SortCreate[ctx_, "Boolean"] := Z3SortObject[ctx, makeBooleanSortC[ctx["RawContext"]]]
iZ3SortCreate[ctx_, "Integer"] := Z3SortObject[ctx, makeIntegerSortC[ctx["RawContext"]]]
iZ3SortCreate[ctx_, "Real"] := Z3SortObject[ctx, makeRealSortC[ctx["RawContext"]]]


(*
	Z3SortObject
*)

Z3SortObject[ctx_Z3ContextObject, rawSort_]["RawSort"] := rawSort
Z3SortObject[ctx_Z3ContextObject, rawSort_]["Context"] := ctx


sortToASTC := sortToASTC =
	ForeignFunctionLoad[$LibZ3, "Z3_sort_to_ast", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

sort_Z3SortObject["AST"] := Z3ASTObject[sort["Context"], sortToASTC[sort["Context"]["RawContext"], sort["RawSort"]]]

sort_Z3SortObject["Hash"] := sort["AST"]["Hash"]


Z3SortObject /: MakeBoxes[sort_Z3SortObject, form:StandardForm]:=
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
