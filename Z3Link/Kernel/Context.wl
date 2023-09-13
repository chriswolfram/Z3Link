BeginPackage["ChristopherWolfram`Z3Link`Context`"];

Z3GetContext

Z3ErrorCodeString
$DefaultExceptionHandler

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]


(*
	Z3 configuration
*)

makeConfigC := makeConfigC =
	ForeignFunctionLoad[$LibZ3, "Z3_mk_config", {} -> "OpaqueRawPointer"];

deleteConfigC := deleteConfigC =
	ForeignFunctionLoad[$LibZ3, "Z3_del_config", {"OpaqueRawPointer"} -> "Void"];

setConfigParamC := setConfigParamC =
	ForeignFunctionLoad[$LibZ3, "Z3_set_param_value", {"OpaqueRawPointer", "RawPointer"::["UnsignedInteger8"], "RawPointer"::["UnsignedInteger8"]} -> "Void"];

makeConfig[opts_?AssociationQ] :=
	With[{config = CreateManagedObject[makeConfigC[], deleteConfigC]},
		KeyValueMap[
			setConfigParamC[config, RawMemoryExport[#1], RawMemoryExport[#2]]&,
			opts
		];
		config
	]


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

(* Enable proof generation by default *)

DeclareFunction[Z3ContextCreate, iZ3ContextCreate, {1,2}];

iZ3ContextCreate[params_?AssociationQ, exceptionHandler_:($DefaultExceptionHandler[##]&), opts_] :=
	Z3ContextObject[
		makeRawContext[makeConfig[params]],
		CreateForeignCallback[exceptionHandler, {"OpaqueRawPointer", "CInt"} -> "Void"]
	]


(*
	Z3GetContext[obj]
		gets the context used by a Z3 object.

	Z3GetContext[obj1, obj2, ...]
		gets the common context used by all of the objects. Returns a Failure if they use different contexts.
*)

(* TODO: Should this return a Failure instead? *)
Z3GetContext[obj_] :=
	Replace[iZ3GetContext[obj], None :> $Z3Context]

Z3GetContext[objs__] :=
	With[{ctxs = DeleteDuplicates[DeleteCases[Z3GetContext /@ {objs}, None]]},
		Switch[Length[ctxs],
			1, First[ctxs],
			0, $Z3Context,
			_, Failure["IncompatibleContexts", <|
				"MessageTemplate" -> "Incompatible Z3 contexts `1` encountered among objects `2`.",
				"MessageParameters" -> {ctxs, {objs}},
				"Contexts" -> ctxs,
				"Objects" -> {objs}
			|>]
		]
	]


iZ3GetContext[ast_Z3ASTObject] := ast["Context"]
iZ3GetContext[sym_Z3SymbolObject] := sym["Context"]
iZ3GetContext[sort_Z3SortObject] := sort["Context"]
iZ3GetContext[decl_Z3FunctionDeclarationObject] := Information[decl, "Context"]
iZ3GetContext[solver_Z3SolverObject] := solver["Context"]
iZ3GetContext[model_Z3ModelObject] := model["Context"]
iZ3GetContext[obj_] := None


(*
	Z3ContextObject
*)

DeclareObject[Z3ContextObject, {
		man_ManagedObject /; MatchQ[man["Value"], _OpaqueRawPointer],
		errorHandler_ManagedObject /; MatchQ[errorHandler["Value"], _ForeignCallback]
	}];

(*
	Accessors
*)

context_Z3ContextObject["RawContext"] := context[[1]]
context_Z3ContextObject["ExceptionHandler"] := context[[2]]


(*
	Summary boxes
*)

DeclareObjectFormatting[Z3ContextObject,
	ctx |-> {
		None,
		{
			{"raw context", ctx["RawContext"]}
		},
		{
			{"exception handler: ", ctx["ExceptionHandler"]}
		}
	}]


(*
	Exception handling
*)

(* Z3ErrorCodeString *)

getErrorMessageC := getErrorMessageC =
	ForeignFunctionLoad[$LibZ3, "Z3_get_error_msg", {"OpaqueRawPointer", "CInt"} -> "RawPointer"::["UnsignedInteger8"]];

Z3ErrorCodeString[rawCtx_OpaqueRawPointer, code_] :=
	RawMemoryImport[getErrorMessageC[rawCtx, code], "String"]


(* $DefaultExceptionHandler *)

$DefaultExceptionHandler = Message[Z3ContextObject::err, Z3ErrorCodeString[#1,#2]]&;


$Z3Context := $Z3Context = Z3ContextCreate[<|"proof" -> "true"|>]



End[];
EndPackage[];
