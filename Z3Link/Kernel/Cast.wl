BeginPackage["ChristopherWolfram`Z3Link`Cast`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]
Needs["ChristopherWolfram`Z3Link`Context`"]
Needs["ChristopherWolfram`Z3Link`ConstantsMap`"]


Options[ToZ3] = {
	Z3Context :> $Z3Context
};

ToZ3[args___] :=
	With[{res = ArgumentsOptions[ToZ3[args], {1,2}, <|"Head" -> Hold, "OptionsMode" -> "Shortest"|>]},
		If[FailureQ[res], res, iToZ3@@res]
	]


iToZ3[Hold[ast_Z3ASTObject, sort_Z3SortObject], opts_] /; ast["Sort"]["Hash"] === sort["Hash"] :=
	ast

iToZ3[Hold[ast_Z3ASTObject, sort_Z3SortObject], opts_] :=
	rawCast[ast, sort, ast["Sort"]["Kind"] -> sort["Kind"]]

iToZ3[Hold[ast_Z3ASTObject, Automatic], opts_] :=
	ast

iToZ3[Hold[expr_, sort_Z3SortObject], opts_] :=
	parseExpr[Z3GetContext[sort], expr, sort]

iToZ3[Hold[expr_, Automatic], opts_] :=
	parseExpr[contextFromOpts[opts], expr, Automatic]

iToZ3[Hold[expr_], opts_] :=
	iToZ3[Hold[expr, Automatic], opts]

iToZ3[Hold[ast_, sortExpr:Except[_Z3SortObject | Automatic]], opts_] :=
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



rawCast[ast_, sort_, "Boolean" -> "Integer"] :=
	Z3If[ast, Z3Numeral[1, sort], Z3Numeral[0, sort]]

rawCast[ast_, sort_, "Boolean" -> "Real"] :=
	Z3If[ast, Z3Numeral[1, sort], Z3Numeral[0, sort]]

rawCast[ast_, sort_, sourceKind_ -> targetKind_] :=
	invalidSortError[sourceKind, targetKind, <|"SourceAST" -> ast, "TargetSort" -> sort|>]


parseExpr[ctx_, num_Integer, sort_] /; MatchQ[sort["Kind"], "Integer" | "Real"] :=
	Z3Numeral[num, sort]

parseExpr[ctx_, num_Integer, Automatic] :=
	Z3Numeral[num, Z3SortCreate["Integer", Z3Context -> ctx]]


parseExpr[ctx_, True, sort_] /; sort === Automatic || sort["Kind"] === "Boolean" :=
	Z3True[Z3Context -> ctx]

parseExpr[ctx_, False, sort_] /; sort === Automatic || sort["Kind"] === "Boolean" :=
	Z3False[Z3Context -> ctx]


parseExpr[ctx_, sym_Symbol, sort:Except[Automatic]] :=
	Z3Constant[Z3SymbolCreate[ToString[sym]], sort]

parseExpr[ctx_, expr_, sort_] :=
	invalidExprError[expr, sort]


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


End[];
EndPackage[];
