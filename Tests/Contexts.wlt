Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	MatchQ[$Z3Context, _Z3ContextObject]
]

TestCreate[
	MatchQ[Z3ContextCreate[<||>], _Z3ContextObject]
]

TestCreate[
	MatchQ[Z3ContextCreate[<|"proof" -> "true"|>], _Z3ContextObject]
]


Needs["ChristopherWolfram`Z3Link`Context`"]

(* Z3GetContext *)

TestCreate[
	MatchQ[Z3GetContext[Z3True[]], _Z3ContextObject]
]

TestCreate[
	MatchQ[Z3GetContext[Z3SymbolCreate["x"]], _Z3ContextObject]
]

TestCreate[
	MatchQ[Z3GetContext[Z3SortCreate["Integer"]], _Z3ContextObject]
]

TestCreate[
	MatchQ[Z3GetContext[Z3Not[Z3True[]]["Head"]], _Z3ContextObject]
]

TestCreate[
	MatchQ[Z3GetContext[Z3SolverCreate[]], _Z3ContextObject]
]

(* Missing: a test for Z3ModelObjects *)


TestCreate[
	MatchQ[Z3GetContext[
		Z3True[],
		Z3SymbolCreate["x"],
		Z3SortCreate["Integer"],
		Z3GetContext[Z3Not[Z3True[]]["Head"]],
		Z3SolverCreate[]
	], _Z3ContextObject]
]

TestCreate[
	FailureQ@Z3GetContext[
		Z3True[Z3Context -> Z3ContextCreate[<||>]],
		Z3SymbolCreate["x"],
		Z3SortCreate["Integer"],
		Z3GetContext[Z3Not[Z3True[]]["Head"]],
		Z3SolverCreate[]
	]
]
