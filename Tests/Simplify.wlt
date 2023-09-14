Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	MatchQ[Z3Simplify[Z3Power[Z3Numeral[2], Z3Numeral[2]]], _Z3ASTObject]
]

TestCreate[
	Z3Simplify[Z3Power[Z3Numeral[2], Z3Numeral[2]]]["String"]
	,
	"4.0"
]