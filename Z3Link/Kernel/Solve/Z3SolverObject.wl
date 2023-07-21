BeginPackage["ChristopherWolfram`Z3Link`Solve`Z3SolverObject`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


makeSolverC := makeSolverC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_solver", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

incSolverRefC := incSolverRefC =
	ForeignFunctionLoad[$LibZ3, "Z3_solver_inc_ref", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

decSolverRefC := decSolverRefC =
	ForeignFunctionLoad[$LibZ3, "Z3_solver_dec_ref", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];


(*
	Z3SolverCreate
*)

Options[Z3SolverCreate] = {Z3Context :> $Z3Context};

Z3SolverCreate[opts:OptionsPattern[]] := iZ3SolverCreate[OptionValue[Z3Context]]

iZ3SolverCreate[ctx_] :=
	With[{rawSolver = makeSolverC[ctx["RawContext"]]},
		incSolverRefC[ctx["RawContext"], rawSolver];
		Z3SolverObject[ctx, CreateManagedObject[rawSolver, decSolverRefC[ctx,#]&]]
	]


(*
	Z3SolverObject
*)

Z3SolverObject[args___] /; !argumentsZ3SolverObject[args] :=
	With[{res = ArgumentsOptions[Z3SolverObject[args], 2]},
		If[FailureQ[res],
			res,
			Message[Z3SolverObject::inv, {args}];
			Failure["InvalidZ3SolverObject", <|
				"MessageTemplate" :> Z3SolverObject::inv,
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>]
		]
	]

argumentsZ3SolverObject[_Z3ContextObject, man_ManagedObject /; MatchQ[man["Value"], _OpaqueRawPointer]] := True
argumentsZ3SolverObject[___] := False


(*
	Accessors
*)

HoldPattern[Z3SolverObject][ctx_Z3ContextObject, rawSolver_]["RawSolver"] := rawSolver
HoldPattern[Z3SolverObject][ctx_Z3ContextObject, rawSolver_]["Context"] := ctx


(*
	Summary box
*)

Z3SolverObject /: MakeBoxes[solver:Z3SolverObject[args___] /; argumentsZ3SolverObject[args], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3SolverObject,
		solver,
		None,
		{BoxForm`SummaryItem@{"raw solver: ", solver["RawSolver"]}},
		{},
		form
	]


End[];
EndPackage[];
