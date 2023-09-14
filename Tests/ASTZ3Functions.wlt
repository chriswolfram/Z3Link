Needs["ChristopherWolfram`Z3Link`"]


(* Z3Numeral *)

TestCreate[
	Z3Numeral[2]["String"]
	,
	"2"
]

TestCreate[
	Z3Numeral[2, Z3SortCreate["Real"]]["String"]
	,
	"2.0"
]

TestCreate[
	Z3Numeral[22/7]["String"]
	,
	"(/ 22.0 7.0)"
]


(* Arithmetic *)

TestCreate[
	Z3Plus[2]["String"]
	,
	"(+ 2)"
]

TestCreate[
	Z3Plus[2, 3, 4]["String"]
	,
	"(+ 2 3 4)"
]

TestCreate[
	Z3Plus[2, 3, True]["String"]
	,
	"(+ 2 3 (ite true 1 0))"
]

TestCreate[
	MatchQ[Z3Plus[2, 3, 2/3], _Z3ASTObject]
]


TestCreate[
	MatchQ[Z3Subtract[2, 3, 4], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Subtract[2, 3, 4], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Subtract[2, 3, True], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Subtract[2, 3, 2/3], _Z3ASTObject]
]


TestCreate[
	Z3Times[2]["String"]
	,
	"(* 2)"
]

TestCreate[
	Z3Times[2, 3, 4]["String"]
	,
	"(* 2 3 4)"
]

TestCreate[
	Z3Times[2, 3, True]["String"]
	,
	"(* 2 3 (ite true 1 0))"
]

TestCreate[
	MatchQ[Z3Times[2, 3, 2/3], _Z3ASTObject]
]


TestCreate[
	Z3Minus[2]["String"]
	,
	"(- 2)"
]

TestCreate[
	Z3Minus[Z3Divide[2, 3]]["String"]
	,
	"(- (/ 2.0 3.0))"
]


TestCreate[
	Z3Divide[2, 3]["String"]
	,
	"(/ 2.0 3.0)"
]

TestCreate[
	Z3Quotient[2, 3]["String"]
	,
	"(div 2 3)"
]

TestCreate[
	FailureQ@Z3Quotient[2, 3/4]
]


TestCreate[
	MatchQ[Z3Divisible[2, 3], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Divisible[2, 3/4]
]

TestCreate[
	FailureQ@Z3Divisible[2, Z3True[]]
]


testTwoArgNumeric[func_] :=
	(
		TestCreate[
			MatchQ[func[2, 3], _Z3ASTObject]
		];

		TestCreate[
			MatchQ[func[2, 3/4], _Z3ASTObject]
		];

		TestCreate[
			MatchQ[func[3/4, 2], _Z3ASTObject]
		];

		TestCreate[
			MatchQ[func[3/4, 1/2], _Z3ASTObject]
		];

		(* Make sure it doesn't crash when given the wrong sorts *)
		TestCreate[
			MatchQ[func[Z3True[], Z3False[]], _Failure | _Z3ASTObject]
		];
	)


(* Logic *)

TestCreate[
	MatchQ[Z3True[], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3False[], _Z3ASTObject]
]


TestCreate[
	MatchQ[Z3Equal[2, 3], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Equal[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Equal[Z3True[], Z3Numeral[2]]
]


TestCreate[
	MatchQ[Z3Distinct[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Distinct[1, 2], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Distinct[Z3True[], 2]
]


TestCreate[
	MatchQ[Z3Not[Z3True[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Not[12]
]


TestCreate[
	MatchQ[Z3If[Z3True[], 1, 2], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3If[0, 1, 2]
]


TestCreate[
	MatchQ[Z3Equivalent[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Equivalent[Z3True[], 2]
]


TestCreate[
	MatchQ[Z3Implies[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Implies[Z3True[], 2]
]


TestCreate[
	MatchQ[Z3Xor[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Xor[Z3True[], 2]
]


TestCreate[
	MatchQ[Z3And[Z3True[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3And[Z3True[], Z3True[], Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3And[Z3True[], Z3True[], Z3True[], 2]
]


TestCreate[
	MatchQ[Z3Or[Z3True[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Or[Z3True[], Z3True[], Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Or[Z3True[], Z3True[], Z3True[], 2]
]


(* Quantifiers *)

TestCreate[
	With[{x = Z3Constant[Z3SymbolCreate["x"], Z3SortCreate["Integer"]]},
		MatchQ[Z3ForAll[x, Z3Equal[x, Z3Numeral[2]]], _Z3ASTObject]
	]
]

TestCreate[
	Clear[x];
	MatchQ[Z3ForAll[x, Z3Equal[x, Z3Numeral[2]/Z3Numeral[3]]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3ForAll[{x,y}, Z3Equal[x, y]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3ForAll[Z3Constant[Z3SymbolCreate["x"], Z3SortCreate["Boolean"]], Z3Constant[Z3SymbolCreate["x"], Z3SortCreate["Boolean"]]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3ForAll[x, Z3Numeral[2]]
]


TestCreate[
	With[{x = Z3Constant[Z3SymbolCreate["x"], Z3SortCreate["Integer"]]},
		MatchQ[Z3Exists[x, Z3Equal[x, Z3Numeral[2]]], _Z3ASTObject]
	]
]

TestCreate[
	Clear[x];
	MatchQ[Z3Exists[x, Z3Equal[x, Z3Numeral[2]/Z3Numeral[3]]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Exists[{x,y}, Z3Equal[x, y]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Exists[Z3Constant[Z3SymbolCreate["x"], Z3SortCreate["Boolean"]], Z3Constant[Z3SymbolCreate["x"], Z3SortCreate["Boolean"]]], _Z3ASTObject]
]

TestCreate[
	FailureQ@Z3Exists[x, Z3Numeral[2]]
]


(* TODO: Sets *)