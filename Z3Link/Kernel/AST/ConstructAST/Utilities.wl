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

		isym[Hold[args___Z3ASTObject], opts_] :=
			Enclose@Module[{ctx},
				ctx = Confirm@Z3GetContext[args];
				(* TODO: Add better message *)
				ConfirmAssert[TrueQ[argCheck[args]]];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Sequence@@(#["RawAST"]&/@{args})]]
			];

		isym[Hold[args___], opts_] :=
			Failure["InvalidArguments", <|
				"MessageTemplate" :> "Expected a sequence of Z3ASTObjects but found `1` instead.",
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>];

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

		isym[Hold[args___Z3ASTObject], opts_] :=
			Enclose@Module[{ctx, argArray},
				ctx = Confirm@Z3GetContext[args];
				(* TODO: Add better message *)
				ConfirmAssert[TrueQ[argCheck[args]]];
				argArray = RawMemoryExport[#["RawAST"] &/@ {args}, "OpaqueRawPointer"];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Length[{args}], argArray]]
			];

		isym[Hold[args___], opts_] :=
			Failure["InvalidArguments", <|
				"MessageTemplate" :> "Expected a sequence of Z3ASTObjects but found `1` instead.",
				"MessageParameters" -> {{args}},
				"Arguments" -> {args}
			|>];

	]


End[];
EndPackage[];
