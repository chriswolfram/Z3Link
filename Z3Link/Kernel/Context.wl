BeginPackage["ChristopherWolfram`Z3Link`Context`"];

Z3GetContext

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


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
	Z3ContextCreate
*)

(* TODO: Check that the context pointer is valid *)

Z3ContextCreate[] := Z3ContextObject[makeRawContext[makeConfig[]]]


(*
	Z3GetContext[obj]
		gets the context used by a Z3 object.

	Z3GetContext[obj1, obj2, ...]
		gets the common context used by all of the objects. Returns a Failure if they use different contexts.
*)

Z3GetContext[ast_Z3ASTObject] := ast["Context"]
Z3GetContext[sym_Z3SymbolObject] := sym["Context"]
Z3GetContext[sort_Z3SortObject] := sort["Context"]
Z3GetContext[decl_Z3FunctionDeclarationObject] := decl["Context"]

(* TODO: Should this return a Failure instead? *)
Z3GetContext[obj_] := $Z3Context

Z3GetContext[objs__] :=
	With[{ctxs = DeleteDuplicates[Z3GetContext /@ {objs}]},
		If[Length[ctxs] === 1,
			First[ctxs],
			Failure["IncompatibleContexts", <|
				"MessageTemplate" -> "Incompatible Z3 contexts `1` encountered among objects `2`.",
				"MessageParameters" -> {ctxs, {objs}},
				"Contexts" -> ctxs,
				"Objects" -> {objs}
			|>]
		]
	]


(*
	Z3ContextObject
*)

Z3ContextObject[rawCtx_]["RawContext"] := rawCtx

Z3ContextObject /: MakeBoxes[ctx_Z3ContextObject, form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		Z3ContextObject,
		ctx,
		None,
		{BoxForm`SummaryItem@{"raw context: ", ctx["RawContext"]}},
		{},
		form
	]


$Z3Context := $Z3Context = Z3ContextCreate[]



End[];
EndPackage[];
