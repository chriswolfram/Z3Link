BeginPackage["ChristopherWolfram`Z3Link`Context`"];

GetCommonContext

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


(*
	Z3 configuration
*)

makeConfigC := makeConfigC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_config", {} -> "OpaqueRawPointer"];

deleteConfigC := deleteConfigC =
	ForeignFunctionLoad[$LibZ3, "Z3_del_config", {"OpaqueRawPointer"} -> "Void"];

makeConfig[] := CreateManagedObject[makeConfigC[], deleteConfigC]


(*
	Z3 context
*)

makeContextC := makeContextC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_context", {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

deleteContextC := deleteContextC =
	ForeignFunctionLoad[$LibZ3, "Z3_del_context", {"OpaqueRawPointer"} -> "Void"];

makeRawContext[config_] := CreateManagedObject[makeContextC[config], deleteConfigC]


(*
	CreateZ3Context
*)

(* TODO: Check that the context pointer is valid *)

CreateZ3Context[] := Z3ContextObject[makeRawContext[makeConfig[]]]


(*
	GetCommonContext[arg1, arg2, ...]
		gets the common context used by all of the arguments. Returns a Failure if they use different contexts.
*)

(* GetCommonContext[args__] := *)
	


(*
	Z3ContextObject
*)

Z3ContextObject[rawCtx_]["RawContext"] := rawCtx

Z3ContextObject /: MakeBoxes[ctx_Z3ContextObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3ContextObject,
		ctx,
		None,
		{"raw context: ", ctx["RawContext"]},
		{},
		form
	]


$Z3Context := $Z3Context = CreateZ3Context[]



End[];
EndPackage[];
