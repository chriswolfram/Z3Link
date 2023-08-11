BeginPackage["ChristopherWolfram`Z3Link`ASTVector`"];

Z3ASTVectorCreate
Z3ASTVectorObject

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


(*
	Z3ASTVectorCreate
*)

vectorIncRefC := vectorIncRefC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_vector_inc_ref", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "Void"];

vectorDecRefC := vectorDecRefC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_vector_dec_ref", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "Void"];


Options[Z3ASTVectorCreate] = {};

Z3ASTVectorCreate[args___] :=
	With[{res = ArgumentsOptions[Z3ASTVectorCreate[args], 2, <|"Head" -> Hold|>]},
		If[FailureQ[res], res, iZ3ASTVectorCreate@@res]
	]

(* TODO: Add fallthrough *)
iZ3ASTVectorCreate[Hold[ctx_Z3ContextObject, ptr_OpaqueRawPointer], Hold[opts___]] :=
	With[{man = CreateManagedObject[ptr, vectorDecRefC[ctx["RawContext"], #]&]},
		vectorIncRefC[ctx["RawContext"], ptr];
		Z3ASTVectorObject[ctx, man]
	]


(*
	Z3ASTVectorObject
*)

Z3ASTVectorObject[args___] /; !argumentsZ3ASTVectorObject[args] :=
	With[{res = ArgumentsOptions[Z3ASTVectorObject[args], 2]},
		If[FailureQ[res],
			res,
			Message[Z3ASTVectorObject::inv, {args}];
			Failure["InvalidZ3ASTVectorObject", <|
				"MessageTemplate" :> Z3ASTVectorObject::inv,
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>]
		]
	]

argumentsZ3ASTVectorObject[_Z3ContextObject, man_ManagedObject /; MatchQ[man["Value"], _OpaqueRawPointer]] := True
argumentsZ3ASTVectorObject[___] := False


(*
	Accessors
*)

HoldPattern[Z3ASTVectorObject][ctx_Z3ContextObject, rawASTVector_]["RawASTVector"] := rawASTVector
HoldPattern[Z3ASTVectorObject][ctx_Z3ContextObject, rawASTVector_]["Context"] := ctx


(* Length *)

vectorSizeC := vectorSizeC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_vector_size", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

vec_Z3ASTVectorObject["Length"] := vectorSizeC[vec["Context"]["RawContext"], vec["RawASTVector"]]


(* Part *)

vectorGetC := vectorGetC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_vector_get", {"OpaqueRawPointer", "OpaqueRawPointer", "CUnsignedInt"} -> "OpaqueRawPointer"]; 

vec_Z3ASTVectorObject["Part", i_Integer?Positive] :=
	Enclose@Module[{numElems, ctx},
		numElems = vec["Length"];
		ctx = vec["Context"];
		ConfirmAssert[i <= numElems];
		Z3ASTObject[ctx, vectorGetC[ctx["RawContext"], vec["RawASTVector"], i-1]]
	]


(* Elements *)

vec_Z3ASTVectorObject["Elements"] :=
	Enclose@Module[{numElems, ctx},
		numElems = vec["Length"];
		ctx = vec["Context"];
		Z3ASTObject[ctx, vectorGetC[ctx["RawContext"], vec["RawASTVector"], #-1]] &/@ Range[numElems]
	]


(*
	Summary boxes
*)

Z3ASTVectorObject /: MakeBoxes[vec:Z3ASTVectorObject[args___] /; argumentsZ3ASTVectorObject[args], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3ASTVectorObject,
		vec,
		None,
		{BoxForm`SummaryItem@{"raw vector: ", vec["RawASTVector"]}},
		{},
		form
	]


End[];
EndPackage[];
