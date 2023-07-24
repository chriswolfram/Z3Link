BeginPackage["ChristopherWolfram`Z3Link`Solve`SolverOperations`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


(*
	Assert
*)

solverAssertC := solverAssertC =
	ForeignFunctionLoad[$LibZ3, "Z3_solver_assert", {"OpaqueRawPointer", "OpaqueRawPointer", "OpaqueRawPointer"} -> "Void"];

solver_Z3SolverObject["Assert", ast_Z3ASTObject] :=
	Enclose@With[{ctx = Confirm@Z3GetContext[solver, ast]},
		solverAssertC[ctx["RawContext"], solver["RawSolver"], ast["RawAST"]];
		solver
	]


(*
	Check
*)

$liftedBooleanMapping := $liftedBooleanMapping = <|
	$Z3ConstantsMap["LiftedBooleans"]["Z3_L_FALSE"] -> False,
	$Z3ConstantsMap["LiftedBooleans"]["Z3_L_UNDEF"] -> Undefined,
	$Z3ConstantsMap["LiftedBooleans"]["Z3_L_TRUE"] -> True
|>;

solverCheckC := solverCheckC =
	ForeignFunctionLoad[$LibZ3, "Z3_solver_check", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CInt"];

solver_Z3SolverObject["Check"] :=
	Lookup[$liftedBooleanMapping, solverCheckC[solver["Context"]["RawContext"], solver["RawSolver"]], Undefined]


(*
	Proof
*)

(* TODO: This only works if proofs are enabled. Maybe default to False and use Z3_solver_translate to move to a context with proofs if needed? *)
solverProofC := solverProofC =
	ForeignFunctionLoad[$LibZ3, "Z3_solver_get_proof", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

solver_Z3SolverObject["Proof"] :=
	Z3ASTObject[solver["Context"], solverProofC[solver["Context"]["RawContext"], solver["RawSolver"]]]


(*
	Model
*)

(* TODO: This can throw exceptions if there is no model available. The best solution would be to start catching exceptions everywhere. *)
solverModelC := solverModelC =
	ForeignFunctionLoad[$LibZ3, "Z3_solver_get_model", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

solver_Z3SolverObject["Model"] :=
	Z3ModelObject[solver["Context"], solverModelC[solver["Context"]["RawContext"], solver["RawSolver"]]]



End[];
EndPackage[];
