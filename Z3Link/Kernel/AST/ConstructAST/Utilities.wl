BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"];

DefineASTConstructor

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]


DefineASTConstructor[sym_, name_, argCount_Integer, argCheck_:(True&)] :=
	Module[{ff, isym},

		ff := ff = ForeignFunctionLoad[$LibZ3, name, ConstantArray["OpaqueRawPointer", argCount+1] -> "OpaqueRawPointer"];

		sym[args___] :=
			With[{res = ArgumentsOptions[sym[args], argCount, <|"Head" -> Hold|>]},
				If[FailureQ[res], res, isym@@res]
			];

		isym[Hold[args___], opts_] :=
			Enclose@Module[{ctx},
				ctx = Confirm@Z3GetContext[args];
				z3Args = Confirm@ToZ3[#, Z3Context -> ctx] &/@ {args};
				(* TODO: Add better message *)
				ConfirmAssert[TrueQ[argCheck@@z3Args]];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Sequence@@(#["RawAST"]&/@z3Args)]]
			];

	]


DefineASTConstructor[sym_, name_, 0, argCheck_:(True&)] :=
	Module[{ff, isym},

		ff := ff = ForeignFunctionLoad[$LibZ3, name, {"OpaqueRawPointer"} -> "OpaqueRawPointer"];

		Options[sym] = {Z3Context :> $Z3Context};

		sym[args___] :=
			With[{res = ArgumentsOptions[sym[args], 0, <|"Head" -> Hold|>]},
				If[FailureQ[res], res, isym@@res]
			];

		isym[Hold[], opts_] :=
			With[{ctx = OptionValue[sym, List@@opts, Z3Context]},
				Z3ASTObject[ctx, ff[ctx["RawContext"]]]
			];

	]


DefineASTConstructor[sym_, name_, argCount_, argCheck_:(True&)] :=
	Module[{ff, isym},

		ff := ff = ForeignFunctionLoad[$LibZ3, name, {"OpaqueRawPointer", "CUnsignedInt", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"];

		sym[args___] :=
			With[{res = ArgumentsOptions[sym[args], argCount, <|"Head" -> Hold|>]},
				If[FailureQ[res], res, isym@@res]
			];

		isym[Hold[args___], opts_] :=
			Enclose@Module[{ctx, z3Args, argArray},
				ctx = Confirm@Z3GetContext[args];
				z3Args = Confirm@ToZ3[#, Z3Context -> ctx] &/@ {args};
				(* TODO: Add better message *)
				ConfirmAssert[TrueQ[argCheck@@z3Args]];
				argArray = RawMemoryExport[#["RawAST"] &/@ z3Args, "OpaqueRawPointer"];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Length[z3Args], argArray]]
			];

	]


DefineASTConstructor[sym_, name_, argSorts: {(_?StringQ | _Z3SortObject | Automatic)..}, argCheck_:(True&)] :=
	Module[{ff, isym, argCount},

		argCount = Length[argSorts];

		ff := ff = ForeignFunctionLoad[$LibZ3, name, ConstantArray["OpaqueRawPointer", argCount+1] -> "OpaqueRawPointer"];

		sym[args___] :=
			With[{res = ArgumentsOptions[sym[args], argCount, <|"Head" -> Hold|>]},
				If[FailureQ[res], res, isym@@res]
			];

		isym[Hold[args___], opts_] :=
			Enclose@Module[{ctx, z3Args},
				(* This relies on the fact that Z3GetContext returns $Z3Context when given a non-Z3 object. *)
				ctx = Confirm@Z3GetContext[z3Args];
				z3Args = MapThread[Confirm@ToZ3[#1, #2, Z3Context -> ctx]&, {{args}, argSorts}];
				(* TODO: Add better message *)
				ConfirmAssert[TrueQ[argCheck@@z3Args]];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Sequence@@(#["RawAST"]&/@z3Args)]]
			];

	]


End[];
EndPackage[];
