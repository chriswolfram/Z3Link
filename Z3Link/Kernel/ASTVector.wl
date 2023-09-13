BeginPackage["ChristopherWolfram`Z3Link`ASTVector`"];

Z3ASTVectorCreate
Z3ASTVectorObject

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]


(*
	Z3ASTVectorCreate
*)

vectorIncRefC := vectorIncRefC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_vector_inc_ref", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "Void"];

vectorDecRefC := vectorDecRefC =
	ForeignFunctionLoad[$LibZ3, "Z3_ast_vector_dec_ref", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "Void"];


DeclareFunction[Z3ASTVectorCreate, iZ3ASTVectorCreate, 2];

Options[Z3ASTVectorCreate] = {};

iZ3ASTVectorCreate[ctx_Z3ContextObject, ptr_OpaqueRawPointer, opts_] :=
	With[{man = CreateManagedObject[ptr, vectorDecRefC[ctx["RawContext"], #]&]},
		vectorIncRefC[ctx["RawContext"], ptr];
		Z3ASTVectorObject[ctx, man]
	]


(*
	Z3ASTVectorObject
*)

DeclareObject[Z3ASTVectorObject, {_Z3ContextObject, man_ManagedObject /; MatchQ[man["Value"], _OpaqueRawPointer]}];


(*
	Accessors
*)

vec_Z3ASTVectorObject["Context"] := vec[[1]]
vec_Z3ASTVectorObject["RawASTVector"] := vec[[2]]


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

DeclareObjectFormatting[Z3ASTVectorObject,
	vec |-> {
		None,
		{
			{"raw vector: ", vec["RawASTVector"]}
		},
		{}
	}]


End[];
EndPackage[];
