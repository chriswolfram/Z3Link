Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	MatchQ[Z3Constant[Z3SymbolCreate["x"], Z3SortCreate[Integers]], _Z3ASTObject]
]

(* Graceful failures *)
TestCreate[
	MatchQ[Z3Constant[12, 3], _Failure]
]