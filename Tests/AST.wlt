Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	MatchQ[Z3Power[Z3Numeral[2], Z3Numeral[2]], _Z3ASTObject]
]

TestCreate[
	Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["String"]
	,
	"(^ 2 (+ 2 3))"
]

TestCreate[
	Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Sort"] === Z3SortCreate["Real"]
]

TestCreate[
	MatchQ[Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Hash"], _Integer]
]

TestCreate[
	Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Kind"]
	,
	"Application"
]

TestCreate[
	FailureQ@Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["FunctionDeclaration"]
	,
	True
	,
	{Z3ASTObject::kind}
]

TestCreate[
	MatchQ[Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Head"], _Z3FunctionDeclarationObject]
]

TestCreate[
	Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["ArgumentCount"]
	,
	2
]

TestCreate[
	MatchQ[Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Arguments"], {_Z3ASTObject, _Z3ASTObject}]
]

TestCreate[
	MatchQ[Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Arguments", 1], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Power[Z3Numeral[2], Z3Plus[Z3Numeral[2], Z3Numeral[3]]]["Arguments", 10]
	,
	True
	,
	{Z3ASTObject::arg}
]