Needs["ChristopherWolfram`Z3Link`"]


(* Arithmetic *)

TestCreate[
	MatchQ[Z3Numeral[4] + 2, _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Numeral[4] + 2 + 3 + 4, _Z3ASTObject]
]


TestCreate[
	MatchQ[Z3Numeral[4] - 2, _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Numeral[4] - 2 - 3 - 4, _Z3ASTObject]
]


TestCreate[
	MatchQ[Z3Numeral[4] * 2, _Z3ASTObject]
]

TestCreate[
	MatchQ[Z3Numeral[4] * 2 * 3 * 4, _Z3ASTObject]
]


TestCreate[
	MatchQ[-Z3Numeral[4], _Z3ASTObject]
]


testTwoArgNumericUpValues[func_] := (
		TestCreate[
			MatchQ[func[Z3Numeral[4], Z3Numeral[3]], _Z3ASTObject]
		];

		TestCreate[
			MatchQ[func[Z3Numeral[4], 3], _Z3ASTObject]
		];

		TestCreate[
			MatchQ[func[4, Z3Numeral[3]], _Z3ASTObject]
		]
	)


testTwoArgNumericUpValues[Divide];
testTwoArgNumericUpValues[Mod];
testTwoArgNumericUpValues[Power];
testTwoArgNumericUpValues[Less];
testTwoArgNumericUpValues[LessEqual];
testTwoArgNumericUpValues[Greater];
testTwoArgNumericUpValues[GreaterEqual];
testTwoArgNumericUpValues[Divisible];


(* Logic *)

TestCreate[
	MatchQ[Z3Numeral[4] == Z3Numeral[3], _Z3ASTObject]
]


TestCreate[
	MatchQ[!Z3True[], _Z3ASTObject]
]


TestCreate[
	MatchQ[!Z3True[], _Z3ASTObject]
]


testTwoArgBooleanUpValues[func_] := (
		TestCreate[
			MatchQ[func[Z3True[], Z3False[]], _Z3ASTObject | _?BooleanQ]
		];

		TestCreate[
			MatchQ[func[Z3True[], False], _Z3ASTObject | _?BooleanQ]
		];

		TestCreate[
			MatchQ[func[True, Z3False[]], _Z3ASTObject | _?BooleanQ]
		];

		TestCreate[
			MatchQ[func[Z3True[], True], _Z3ASTObject | _?BooleanQ]
		];

		TestCreate[
			MatchQ[func[False, Z3False[]], _Z3ASTObject | _?BooleanQ]
		];
	)


testTwoArgBooleanUpValues[Equivalent];
testTwoArgBooleanUpValues[Implies];
testTwoArgBooleanUpValues[Xor];
testTwoArgBooleanUpValues[And];
testTwoArgBooleanUpValues[Or];


TestCreate[
	MatchQ[And[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[And[Z3True[], Z3False[], Z3False[], Z3True[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[And[Z3True[], Z3False[], Z3True[], True], _Z3ASTObject | _?BooleanQ]
]


TestCreate[
	MatchQ[Or[Z3True[], Z3False[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Or[Z3True[], Z3False[], Z3False[], Z3True[]], _Z3ASTObject]
]

TestCreate[
	MatchQ[Or[Z3True[], Z3False[], Z3True[], True], _Z3ASTObject | _?BooleanQ]
]


TestCreate[
	MatchQ[Boole[Z3True[]], _Z3ASTObject]
]