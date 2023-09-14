Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	MatchQ[Z3SymbolCreate[1], _Z3SymbolObject]
]

TestCreate[
	MatchQ[Z3SymbolCreate["x"], _Z3SymbolObject]
]

TestCreate[
	FailureQ@Z3SymbolCreate[x+2]
]

TestCreate[
	Z3SymbolCreate["x"]["Name"]
	,
	"x"
]

TestCreate[
	Z3SymbolCreate[1]["Name"]
	,
	1
]