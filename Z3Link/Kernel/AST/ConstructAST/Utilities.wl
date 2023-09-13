BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`Utilities`"];

DeclareZ3Function
CoerceArguments
DeclareASTConstructor
DefineExpressionParsing
ParseExpressionArguments

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`Utilities`"]
Needs["ChristopherWolfram`Z3Link`AST`ConstructAST`ToZ3`"]



(* DeclareZ3Function *)

DeclareZ3Function[wlName_, z3Name_, argCount_] :=
	Module[{ff},
		ff := ff = ForeignFunctionLoad[$LibZ3, z3Name, ConstantArray["OpaqueRawPointer", argCount+1] -> "OpaqueRawPointer"];

		wlName[ctx_, args_] :=
			Z3ASTObject[ctx, ff[ctx["RawContext"], Sequence@@(#["RawAST"]&/@args)]]
	]

DeclareZ3Function[wlName_, z3Name_, Infinity] :=
	Module[{ff},
		ff := ff = ForeignFunctionLoad[$LibZ3, z3Name, {"OpaqueRawPointer", "CUnsignedInt", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"];

		wlName[ctx_, args_] :=
			Module[{argArray},
				argArray = RawMemoryExport[#["RawAST"] &/@ args, "OpaqueRawPointer"];
				Z3ASTObject[ctx, ff[ctx["RawContext"], Length[args], argArray]]
			]
	]


(* CoerceArguments *)

CoerceArguments[ctx_, args_, Automatic] :=
	Enclose[Confirm@ToZ3[#, Z3Context -> ctx] &/@ args]

CoerceArguments[ctx_, args_, sorts_List] :=
	Enclose[MapThread[Confirm@ToZ3[#1, #2, Z3Context -> ctx]&, {args, sorts}]]

CoerceArguments[ctx_, args_, sort_] :=
	Enclose[Confirm@ToZ3[#, sort, Z3Context -> ctx] &/@ args]

CoerceArguments[ctx_, args_] :=
	CoerceArguments[ctx, args, Automatic]


(* DeclareASTConstructor *)


DeclareASTConstructor[sym_, name_, argSpec_, argSorts_:Automatic, argCheck_:(True&)] :=
	Enclose@Module[{argCount, isym, cfunc},

		argCount = Confirm@argSpecCount[argSpec];

		DeclareZ3Function[cfunc, name, argCount];
		DeclareFunction[sym, isym, argSpec];

		If[argCount === 0,

			Options[sym] = {Z3Context :> $Z3Context};
			isym[opts_] :=
				Enclose@Module[{ctx},
					ctx = ConfirmMatch[OptionValue[sym, List@@opts, Z3Context], _Z3ContextObject];
					cfunc[ctx, {}]
				],

			isym[args___, opts_] :=
				Enclose@Module[{ctx, z3Args},
					ctx = Confirm@Z3GetContext[args];
					z3Args = Confirm@CoerceArguments[ctx, {args}, argSorts];
					ConfirmAssert[TrueQ[argCheck@@z3Args]];
					cfunc[ctx, z3Args]
				]
		]

	]


argSpecCount[n_Integer] := n
argSpecCount[{_Integer, _Integer | Infinity}] := Infinity
argSpecCount[spec_] := $Failed



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
