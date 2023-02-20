BeginPackage["ChristopherWolfram`Z3Link`Sort`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`LibZ3`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


makeBooleanSortC := makeBooleanSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_bool_sort", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

(*
	CreateZ3Sort
*)

Options[CreateZ3Sort] = {Z3Context :> $Z3Context};

CreateZ3Sort[sortName_, opts:OptionsPattern[]] := iCreateZ3Sort[OptionValue[Z3Context]["RawContext"], sortName]

(* TODO: Add fallthrough *)
iCreateZ3Sort[ctx_, "Boolean"] := Z3SortObject[makeBooleanSortC[ctx]]


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
