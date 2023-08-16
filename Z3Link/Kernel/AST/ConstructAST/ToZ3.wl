BeginPackage["ChristopherWolfram`Z3Link`AST`ConstructAST`ToZ3`"];

ParseExpression
iParseExpression

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


iToZ3[Hold[ast_Z3ASTObject, sort_Z3SortObject, bindingSorts_], opts_] /; ast["Sort"]["Hash"] === sort["Hash"] :=
	ast

iToZ3[Hold[ast_Z3ASTObject, sort_Z3SortObject, bindingSorts_], opts_] :=
	rawCast[ast, sort, ast["Sort"]["Kind"] -> sort["Kind"]]

iToZ3[Hold[ast_Z3ASTObject, Automatic, bindingSorts_], opts_] :=
	ast

iToZ3[Hold[expr_, sort_Z3SortObject, bindingSorts_], opts_] :=
	Enclose@Module[{ctx},
		ctx = Confirm@Z3GetContext[sort];
		First@Confirm@ParseExpression[ctx, expr, sort, Confirm@normalizeBindings[ctx, bindingSorts]]
	]

iToZ3[Hold[expr_, Automatic, bindingSorts_], opts_] :=
	Enclose@Module[{ctx},
		ctx = Confirm@contextFromOpts[opts];
		First@Confirm@ParseExpression[ctx, expr, Automatic, Confirm@normalizeBindings[ctx, bindingSorts]]
	]

iToZ3[Hold[ast_, sortExpr:Except[_Z3SortObject | Automatic], bindingSorts_], opts_] :=
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



normalizeBindings[ctx_, bindingSorts:{Element[_, _]...}] :=
	Enclose[normalizeBindings[ctx, Association[#1 -> Confirm@normalizeBindingSort[ctx, #2]&@@@bindingSorts]]]

normalizeBindings[ctx_, bindingSorts:Element[_, _]] :=
	normalizeBindings[ctx, {bindingSorts}]

normalizeBindings[ctx_, bindingSorts_?AssociationQ] :=
	Enclose[
		Association@KeyValueMap[
			#1 -> Confirm@Z3Constant[Z3SymbolCreate[ToString[#1]], Confirm@normalizeBindingSort[ctx, #2]]&,
			bindingSorts
		]
	]

normalizeBindings[ctx_, bindingSorts_] :=
	Failure["InvalidBindings", <|
		"MessageTemplate" :> "Unexpected binding specification `1` encountered.",
		"MessageParameters" -> {bindingSorts},
		"BindingSpecification" -> bindingSorts
	|>]



normalizeBindingSort[ctx_, sort_Z3SortObject] :=
	sort

normalizeBindingSort[ctx_, sort_] :=
	Z3SortCreate[sort, Z3Context->ctx]


(*
	rawCast[ast, sort, sourceKind -> targetKind]
		casts AST to from the sourceKind to the targetKind (which matches sort).

		Used for casting one kind of AST to another (for example, Boolean to Integer, etc.) or producing a Failure.
*)


rawCast[ast_, sort_, "Integer" -> "Real"] :=
	Z3IntegerToReal[ast]

rawCast[ast_, sort_, sourceKind_ -> targetKind_] :=
	invalidSortError[sourceKind, targetKind, <|"SourceAST" -> ast, "TargetSort" -> sort|>]



(*
	ParseExpression[ctx, expr, sort, bindings]
		parses expr as an AST.

		ctx: The Z3ContextObject for the resulting AST.
		expr: THe expression.
		sort: Either an Z3SortObject or Automatic.
		bindings: Association mapping symbols to AST objects.

		If an explicit sort is given, but no definition exists for the pair of expr and sort, then
		ParseExpression will be run with the Automatic sort and the result will be checked.
*)

ParseExpression[ctx_, expr_, sort_, bindings_] :=
	With[{parse = iParseExpression[ctx, expr, sort, bindings]},
		If[FailureQ[parse] || (sort =!= Automatic && parse[[1]]["Sort"]["AST"]["Hash"] =!= sort["AST"]["Hash"]),
			invalidExprError[expr, sort],
			parse
		]
	]


(*
	iParseExpression[ctx, expr, sort, bindings]
		used by ParseExpression to parse expr as AST.

	DownValues can be defined for iParseExpression to make more expression types parsable.
*)


(* Fallthrough *)

iParseExpression[ctx_, expr_, sort_, bindings_] :=
	invalidExprError[expr, sort]


(* Integers *)

iParseExpression[ctx_, num_Integer, sort_, bindings_] /; MatchQ[sort["Kind"], "Integer" | "Real"] :=
	{Z3Numeral[num, sort], bindings}

iParseExpression[ctx_, num_Integer, Automatic, bindings_] :=
	{Z3Numeral[num, Z3SortCreate["Integer", Z3Context -> ctx]], bindings}


(* Rationals *)

iParseExpression[ctx_, num_Rational, Automatic, bindings_] :=
	{Z3Numeral[num], bindings}


(* Booleans *)
iParseExpression[ctx_, True, sort_, bindings_] /; sort === Automatic || sort["Kind"] === "Boolean" :=
	{Z3True[Z3Context -> ctx], bindings}

iParseExpression[ctx_, False, sort_, bindings_] /; sort === Automatic || sort["Kind"] === "Boolean" :=
	{Z3False[Z3Context -> ctx], bindings}


(* Symbols *)

iParseExpression[ctx_, sym_Symbol, sort_, bindings_] :=
	If[sort === Automatic,
		If[KeyExistsQ[bindings, sym],
			{bindings[sym], bindings},
			With[{const = Z3Constant[Z3SymbolCreate[ToString[sym]], Z3SortCreate["Real", Z3Context->ctx]]},
				{const, Append[bindings, sym -> const]}
			]
		],
		If[KeyExistsQ[bindings, sym],
			If[sort["AST"]["Hash"] === bindings[sym]["Sort"]["AST"]["Hash"],
				{bindings[sym], bindings},
				Failure["IncompatibleSorts", <|
					"MessageTemplate" :> "The symbol `1` was found with the incompatible sorts `2` and `3`.",
					"MessageParameters" -> {sym, sort, bindings[sym]},
					"Sorts" -> {sort, bindings[sym]}
				|>]
			],
			With[{const = Z3Constant[Z3SymbolCreate[ToString[sym]], sort]},
				{const, Append[bindings, sym -> const]}
			]
		]
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
		"MessageTemplate" :> "Could not automatically convert the expression `1` to a Z3 AST.",
		"MessageParameters" -> {expr},
		"Expression" -> expr,
		extra
	|>]


contextFromOpts[opts_] :=
	OptionValue[ToZ3, List@@opts, Z3Context]



End[];
EndPackage[];
