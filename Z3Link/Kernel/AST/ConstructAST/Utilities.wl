BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"];

DefineASTConstructor
DefineExpressionParsing
ParseExpressionArguments

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`ToZ3`"]


(* AST construction *)

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
				ctx = Confirm@Z3GetContext[args];
				z3Args = MapThread[Confirm@ToZ3[#1, #2, Z3Context -> ctx]&, {{args}, argSorts}];
				(* TODO: Add better message *)
				ConfirmAssert[TrueQ[argCheck@@z3Args]];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Sequence@@(#["RawAST"]&/@z3Args)]]
			];

	]


(* Expression parsing *)

DefineExpressionParsing[head_, sym_] :=
	(
		iParseExpression[ctx_, HoldPattern[head][args___], Automatic, bindings_] :=
			Enclose@Module[{newArgs, newBindings},
				{newArgs, newBindings} = Confirm@ParseExpressionArguments[ctx, {args}, bindings];
				{Confirm[sym@@newArgs], newBindings}
			]
	)

ParseExpressionArguments[ctx_, args_, bindings_] :=
	Enclose@With[{parses = Rest@FoldList[Confirm@ParseExpression[ctx, #2, Automatic, #1[[2]]]&, {None, bindings}, args]},
		{parses[[All,1]], parses[[-1,2]]}
	]


End[];
EndPackage[];
