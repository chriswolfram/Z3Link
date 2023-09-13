BeginPackage["ChristopherWolfram`Z3Link`Solve`Model`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`ASTVector`"]


(*
	Z3ModelObject
*)

DeclareObject[Z3ModelObject, {_Z3ContextObject, _OpaqueRawPointer}];


(*
	Accessors
*)

model_Z3ModelObject["Context"] := model[[1]]
model_Z3ModelObject["RawModel"] := model[[2]]


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


(* Constant *)

modelConstC := modelConstC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_const_decl", {"OpaqueRawPointer", "OpaqueRawPointer", "CUnsignedInt"} -> "OpaqueRawPointer"];

model_Z3ModelObject["Constant", i_Integer?Positive] :=
	Enclose@Module[{numConsts, ctx},
		numConsts = model["ConstantCount"];
		ctx = model["Context"];
		ConfirmAssert[i <= numConsts];
		Z3FunctionDeclarationObject[ctx, modelConstC[ctx["RawContext"], model["RawModel"], i-1]]
	]

model_Z3ModelObject["Constant"] :=
	Enclose@Module[{numConsts, ctx},
		numConsts = model["ConstantCount"];
		ctx = model["Context"];
		Z3FunctionDeclarationObject[ctx, modelConstC[ctx["RawContext"], model["RawModel"], #-1]] &/@ Range[numConsts]
	]


(* Function *)

modelFuncC := modelFuncC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_func_decl", {"OpaqueRawPointer", "OpaqueRawPointer", "CUnsignedInt"} -> "OpaqueRawPointer"];

model_Z3ModelObject["Function", i_Integer?Positive] :=
	Enclose@Module[{numFuncs, ctx},
		numFuncs = model["FunctionCount"];
		ctx = model["Context"];
		ConfirmAssert[i <= numFuncs];
		Z3FunctionDeclarationObject[ctx, modelFuncC[ctx["RawContext"], model["RawModel"], i-1]]
	]

model_Z3ModelObject["Function"] :=
	Enclose@Module[{numFuncs, ctx},
		numFuncs = model["FunctionCount"];
		ctx = model["Context"];
		Z3FunctionDeclarationObject[ctx, modelFuncC[ctx["RawContext"], model["RawModel"], #-1]] &/@ Range[numFuncs]
	]


(* ConstantInterpretation *)

modelConstInterpC := modelConstInterpC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_const_interp", {"OpaqueRawPointer", "OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

model_Z3ModelObject["ConstantInterpretation", const_Z3FunctionDeclarationObject] :=
	Enclose@Module[{ctx},
		ctx = Confirm@Z3GetContext[model, const];
		Z3ASTObject[ctx, modelConstInterpC[ctx["RawContext"], model["RawModel"], Information[const, "RawFunctionDeclaration"]]]
	]

model_Z3ModelObject["ConstantInterpretation"] :=
	AssociationMap[model["ConstantInterpretation", #]&, model["Constant"]]


(* UninterpretedSortCount *)

modelGetNumSortsC := modelGetNumSortsC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_num_sorts", {"OpaqueRawPointer", "OpaqueRawPointer"} -> "CUnsignedInt"];

model_Z3ModelObject["UninterpretedSortCount"] :=
	modelGetNumSortsC[model["Context"]["RawContext"], model["RawModel"]]


(* UninterpretedSort *)

modelGetSortC := modelGetSortC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_sort", {"OpaqueRawPointer", "OpaqueRawPointer", "CUnsignedInt"} -> "OpaqueRawPointer"];

model_Z3ModelObject["UninterpretedSort", i_Integer?Positive] :=
	Enclose@Module[{numSorts, ctx},
		numSorts = model["UninterpretedSortCount"];
		ctx = model["Context"];
		ConfirmAssert[i <= numSorts];
		Z3SortObject[ctx, modelGetSortC[ctx["RawContext"], model["RawModel"], i-1]]
	]

model_Z3ModelObject["UninterpretedSort"] :=
	Enclose@Module[{numSorts, ctx},
		numSorts = model["UninterpretedSortCount"];
		ctx = model["Context"];
		Z3SortObject[ctx, modelGetSortC[ctx["RawContext"], model["RawModel"], #-1]] &/@ Range[numSorts]
	]


(* UninterpretedSortUniverseVector *)

modelGetSortUniverseC := modelGetSortUniverseC =
	ForeignFunctionLoad[$LibZ3, "Z3_model_get_sort_universe", {"OpaqueRawPointer", "OpaqueRawPointer", "OpaqueRawPointer"} -> "OpaqueRawPointer"];

model_Z3ModelObject["UninterpretedSortUniverseVector", sort_Z3SortObject] :=
	Z3ASTVectorCreate[sort["Context"], modelGetSortUniverseC[sort["Context"]["RawContext"], model["RawModel"], sort["RawSort"]]]


(* UninterpretedSortUniverse *)

model_Z3ModelObject["UninterpretedSortUniverse", sort_Z3SortObject] :=
	model["UninterpretedSortUniverseVector", sort]["Elements"]


(*
	Summary boxes
*)

DeclareObjectFormatting[Z3ModelObject,
	model |-> {
		None,
		{
			{"", model["String"]}
		},
		{
			{"raw model: ", model["RawModel"]}
		}
	}]


End[];
EndPackage[];
