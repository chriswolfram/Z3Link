Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	MatchQ[Z3SortCreate["Integer"], _Z3SortObject]
]

TestCreate[
	Z3SortCreate["Integer"]["Hash"] === Z3SortCreate["Integer"]["Hash"]
]

TestCreate[
	Z3SortCreate["Integer"]["Hash"] =!= Z3SortCreate["Real"]["Hash"]
]


(* Built-in sorts *)

TestCreate[
	MatchQ[Z3SortCreate["Integer"], _Z3SortObject]
]

TestCreate[
	MatchQ[Z3SortCreate["Real"], _Z3SortObject]
]

TestCreate[
	MatchQ[Z3SortCreate["Boolean"], _Z3SortObject]
]

TestCreate[
	MatchQ[Z3SortCreate[Integers], _Z3SortObject]
]

TestCreate[
	MatchQ[Z3SortCreate[Reals], _Z3SortObject]
]

TestCreate[
	MatchQ[Z3SortCreate[Booleans], _Z3SortObject]
]

(* Set sorts *)
TestCreate[
	MatchQ[Z3SortCreate[Z3SortCreate["Integer"]], _Z3SortObject]
]

TestCreate[
	Z3SortCreate[Z3SortCreate["Integer"]]["Hash"] =!= Z3SortCreate["Integer"]["Hash"]
]

(* Uninterpreted sorts *)
TestCreate[
	MatchQ[Z3SortCreate[Z3SymbolCreate["x"]], _Z3SortObject]
]


(* Accessors *)

TestCreate[
	MatchQ[Z3SortCreate["Integer"]["AST"], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3SortCreate["Integer"]["KindID"], _Integer]
]

TestCreate[
	MatchQ[Z3SortCreate["Integer"]["Kind"], _?StringQ]
]

TestCreate[
	MatchQ[Z3SortCreate["Integer"]["Name"], _Z3SymbolObject]
]


(* Graceful failures *)
TestCreate[
	MatchQ[Z3SortCreate[12], _Failure]
]