BeginPackage["ChristopherWolfram`Z3Link`Sort`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeBooleanSortC := makeBooleanSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_bool_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeIntegerSortC := makeIntegerSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_int_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

makeRealSortC := makeRealSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_real_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];


(*
	CreateZ3Sort
*)

Options[CreateZ3Sort] = {Z3Context :> $Z3Context};

CreateZ3Sort[sortName_, opts:OptionsPattern[]] := iCreateZ3Sort[OptionValue[Z3Context]["RawContext"], sortName]

(* TODO: Add fallthrough *)
iCreateZ3Sort[ctx_, "Boolean"] := Z3SortObject[makeBooleanSortC[ctx]]
iCreateZ3Sort[ctx_, "Integer"] := Z3SortObject[makeIntegerSortC[ctx]]
iCreateZ3Sort[ctx_, "Real"] := Z3SortObject[makeRealSortC[ctx]]


(*
	Z3SortObject
*)

Z3SortObject[rawSort_]["RawSort"] := rawSort

Z3SortObject /: MakeBoxes[sort_Z3SortObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3SortObject,
		sort,
		None,
		{"raw sort: ", sort["RawSort"]},
		{},
		form
	]


End[];
EndPackage[];
