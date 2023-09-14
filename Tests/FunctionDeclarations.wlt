Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	func = Z3FunctionDeclarationCreate[
		Z3SymbolCreate["func"],
		{Z3SortCreate["Integer"], Z3SortCreate["Boolean"]}, 
		Z3SortCreate["Boolean"]
	];
	MatchQ[func, _Z3FunctionDeclarationObject]
]


(* Information *)

TestCreate[
	MatchQ[Information[func, "Name"], _Z3SymbolObject]
]

TestCreate[
	Information[func, "ArgumentCount"]
	,
	2
]

TestCreate[
	MatchQ[Information[func, "Domain"], {_Z3SortObject, _Z3SortObject}]
]

TestCreate[
	MatchQ[Information[func, "Range"], _Z3SortObject]
]

TestCreate[
	Information[func, "RawKind"]
	,
	"Z3_OP_UNINTERPRETED"
]


(* Application *)

TestCreate[
	MatchQ[func[Z3Numeral[1], Z3True[]], _Z3ASTObject]
]

TestCreate[
	FailureQ@func[Z3True[], Z3True[]]
	,
	True
	,
	Z3FunctionDeclarationObject::appargsort
]

TestCreate[
	FailureQ@func[Z3Numeral[1], Z3True[], Z3True[]]
	,
	True
	,
	Z3FunctionDeclarationObject::appargcount
]

(* TODO: Add better error when applied to invalid inputs *)
(* TestCreate[
	FailureQ@func[1,2]
] *)