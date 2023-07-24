BeginPackage["ChristopherWolfram`Z3Link`Solve`Model`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


(*
	Z3ModelObject
*)

Z3ModelObject[args___] /; !argumentsZ3ModelObject[args] :=
	With[{res = ArgumentsOptions[Z3ModelObject[args], 2]},
		If[FailureQ[res],
			res,
			Message[Z3ModelObject::inv, {args}];
			Failure["InvalidZ3ModelObject", <|
				"MessageTemplate" :> Z3ModelObject::inv,
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>]
		]
	]

argumentsZ3ModelObject[_Z3ContextObject, _OpaqueRawPointer] := True
argumentsZ3ModelObject[___] := False


(*
	Accessors
*)

HoldPattern[Z3ModelObject][ctx_Z3ContextObject, rawModel_]["RawModel"] := rawModel
HoldPattern[Z3ModelObject][ctx_Z3ContextObject, rawModel_]["Context"] := ctx


(* String *)

modelToStringC := modelToStringC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_to_string", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "RawPointer"::["UnsignedInteger8"]];

model_Z3ModelObject["String"] :=
	RawMemoryImport[modelToStringC[model["Context"]["RawContext"], model["RawModel"]], "String"]


(* ConstantCount *)

modelNumConstsC := modelNumConstsC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_num_consts", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

model_Z3ModelObject["ConstantCount"] :=
	modelNumConstsC[model["Context"]["RawContext"], model["RawModel"]]


(* FunctionCount *)

modelNumFuncsC := modelNumFuncsC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_num_funcs", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

model_Z3ModelObject["FunctionCount"] :=
	modelNumFuncsC[model["Context"]["RawContext"], model["RawModel"]]


(*
	Summary boxes
*)

Z3ModelObject /: MakeBoxes[model:Z3ModelObject[args___] /; argumentsZ3ModelObject[args], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3ModelObject,
		model,
		None,
		{BoxForm`SummaryItem@{"", model["String"]}},
		{
			BoxForm`SummaryItem@{"raw model: ", model["RawModel"]}
		},
		form
	]


End[];
EndPackage[];
