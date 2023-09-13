BeginPackage["ChristopherWolfram`Z3Link`Solve`Z3SolverObject`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]
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

DeclareFunction[Z3SolverCreate, iZ3SolverCreate, 0];

Options[Z3SolverCreate] = {Z3Context :> $Z3Context};

iZ3SolverCreate[opts_] := iiZ3SolverCreate[OptionValue[Z3SolverCreate, opts, Z3Context]]

iiZ3SolverCreate[ctx_] :=
	With[{rawSolver = makeSolverC[ctx["RawContext"]]},
		incSolverRefC[ctx["RawContext"], rawSolver];
		Z3SolverObject[ctx, CreateManagedObject[rawSolver, decSolverRefC[ctx,#]&]]
	]


(*
	Z3SolverObject
*)

DeclareObject[Z3SolverObject, {_Z3ContextObject, man_ManagedObject /; MatchQ[man["Value"], _OpaqueRawPointer]}];


(*
	Accessors
*)

solver_Z3SolverObject["Context"] := solver[[1]]
solver_Z3SolverObject["RawSolver"] := solver[[2]]


(*
	Summary box
*)

DeclareObjectFormatting[Z3SolverObject,
	solver |-> {
		None,
		{
			{"raw solver: ", solver["RawSolver"]}
		},
		{}
	}]


End[];
EndPackage[];
