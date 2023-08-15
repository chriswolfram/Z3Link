BeginPackage["ChristopherWolfram`Z3Link`Cast`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


Options[ToZ3] = {
	Z3Context :> $Z3Context
};

ToZ3[args___] :=
	With[{res = ArgumentsOptions[ToZ3[args], {1,3}, <|"Head" -> Hold, "OptionsMode" -> "Shortest"|>]},
		If[FailureQ[res], res, iToZ3@@res]
	]


iToZ3[Hold[ast_Z3ASTObject, sort_Z3SortObject, bindings_], opts_] /; ast["Sort"]["Hash"] === sort["Hash"] :=
	ast

iToZ3[Hold[ast_Z3ASTObject, sort_Z3SortObject, bindings_], opts_] :=
	rawCast[ast, sort, ast["Sort"]["Kind"] -> sort["Kind"]]

iToZ3[Hold[ast_Z3ASTObject, Automatic, bindings_], opts_] :=
	ast

iToZ3[Hold[expr_, sort_Z3SortObject, bindings_], opts_] :=
	Enclose@Module[{ctx},
		ctx = Confirm@Z3GetContext[sort];
		First@Confirm@parseExpr[ctx, expr, sort, Confirm@Z3SortCreate[#, Z3Context -> ctx]&/@bindings]
	]

iToZ3[Hold[expr_, Automatic, bindings_], opts_] :=
	Enclose@Module[{ctx},
		ctx = Confirm@contextFromOpts[opts];
		First@Confirm@parseExpr[ctx, expr, Automatic, Confirm@Z3SortCreate[#, Z3Context -> ctx]&/@bindings]
	]

iToZ3[Hold[ast_, sortExpr:Except[_Z3SortObject | Automatic], bindings_], opts_] :=
	Enclose@Module[{ctx},
		ctx = If[MatchQ[ast, _Z3ASTObject],
			Confirm@Z3GetContext[ast, contextFromOpts[opts]],
			contextFromOpts[opts]
		];
		iToZ3[Hold[Evaluate[
			ast,
			Confirm@Z3SortCreate[sortExpr, Z3Context -> ctx]
		]], opts]
	]

iToZ3[Hold[expr_, sort_], opts_] :=
	iToZ3[Hold[expr, sort, <||>], opts]

iToZ3[Hold[expr_], opts_] :=
	iToZ3[Hold[expr, Automatic], opts]



(* rawCast[ast_, sort_, "Boolean" -> "Integer"] :=
	Z3If[ast, Z3Numeral[1, sort], Z3Numeral[0, sort]]

rawCast[ast_, sort_, "Boolean" -> "Real"] :=
	Z3If[ast, Z3Numeral[1, sort], Z3Numeral[0, sort]] *)

rawCast[ast_, sort_, sourceKind_ -> targetKind_] :=
	invalidSortError[sourceKind, targetKind, <|"SourceAST" -> ast, "TargetSort" -> sort|>]


parseExpr[ctx_, num_Integer, sort_, bindings_] /; MatchQ[sort["Kind"], "Integer" | "Real"] :=
	{Z3Numeral[num, sort], bindings}

parseExpr[ctx_, num_Integer, Automatic, bindings_] :=
	{Z3Numeral[num, Z3SortCreate["Integer", Z3Context -> ctx]], bindings}


(* TODO: Add return sort checking *)
parseExpr[ctx_, num_Rational, Automatic, bindings_] :=
	{Z3Numeral[num], bindings}


parseExpr[ctx_, True, sort_, bindings_] /; sort === Automatic || sort["Kind"] === "Boolean" :=
	{Z3True[Z3Context -> ctx], bindings}

parseExpr[ctx_, False, sort_, bindings_] /; sort === Automatic || sort["Kind"] === "Boolean" :=
	{Z3False[Z3Context -> ctx], bindings}


parseExpr[ctx_, sym_Symbol, sort:Except[Automatic], bindings_] :=
	If[KeyExistsQ[bindings, sym] && sort["AST"]["Hash"] =!= bindings[sym]["Sort"]["AST"]["Hash"],
		Failure["IncompatibleSorts", <|
			"MessageTemplate" :> "The symbol `1` was found with the incompatible sorts `2` and `3`.",
			"MessageParameters" -> {sym, sort, bindings[sym]},
			"Sorts" -> {sort, bindings[sym]}
		|>],
		{Z3Constant[Z3SymbolCreate[ToString[sym]], sort], Append[bindings, sym -> sort]}
	]
	

parseExpr[ctx_, sym_Symbol, Automatic, bindings_] /; KeyExistsQ[bindings, sym] :=
	{Z3Constant[Z3SymbolCreate[ToString[sym]], bindings[sym]], bindings}

parseExpr[ctx_, expr_, sort_, bindings_] :=
	invalidExprError[expr, sort]


(* Arithmetic *)

parseExpr[ctx_, HoldPattern[Plus][args__], Automatic, bindings_] :=
	Enclose@With[{argParse = Confirm@parseArgs[ctx, {args}, bindings]},
		{Z3Plus@@argParse[[1]], argParse[[2]]}
	]

parseExpr[ctx_, HoldPattern[Times][args__], Automatic, bindings_] :=
	Enclose@With[{argParse = Confirm@parseArgs[ctx, {args}, bindings]},
		{Z3Times@@argParse[[1]], argParse[[2]]}
	]

(* Logic *)

(* TODO: Use the fact that Implies knows the types of its arguments *)
parseExpr[ctx_, HoldPattern[Implies][lhs_, rhs_], Automatic, bindings_] :=
	Enclose@With[{argParse = Confirm@parseArgs[ctx, {lhs, rhs}, bindings]},
		{Z3Implies[argParse[[1,1]], argParse[[1,2]]], argParse[[2]]}
	]



(* Utilities *)

invalidSortError[sourceKind_, targetKind_, extra_:<||>] :=
	Failure["InvalidSort", <|
		"MessageTemplate" :> "Expected an expression with sort `1` but found one with sort `2` instead.",
		"MessageParameters" -> {targetKind, sourceKind},
		extra
	|>]


invalidExprError[expr_, sort_, extra_:<||>] :=
	Failure["InvalidExpressionSort", <|
		"MessageTemplate" :> "Expected an expression with sort `1` but found `2` instead.",
		"MessageParameters" -> {sort["Kind"], expr},
		"Expression" -> expr,
		"Sort" -> sort,
		extra
	|>]

invalidExprError[expr_, Automatic, extra_:<||>] :=
	Failure["InvalidExpressionSort", <|
		"MessageTemplate" :> "Could not automatically determine the sort of expression `1`.",
		"MessageParameters" -> {expr},
		"Expression" -> expr,
		extra
	|>]


contextFromOpts[opts_] :=
	OptionValue[ToZ3, List@@opts, Z3Context]


parseArgs[ctx_, args_, bindings_] :=
	Enclose@With[{parses = Rest@FoldList[Confirm@parseExpr[ctx, #2, Automatic, #1[[2]]]&, {None, bindings}, args]},
		{parses[[All,1]], parses[[-1,2]]}
	]


End[];
EndPackage[];
