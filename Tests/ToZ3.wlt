Needs["ChristopherWolfram`Z3Link`"]


TestCreate[
	StringDelete[ToZ3[(-b + Sqrt[b^2 - 4 a c]) / (2 a)]["String"], Whitespace],
	"(*(/1.02.0)(^a(to_real(-1)))(+(*(to_real(-1))b)(^(+(^b(to_real2))(*(to_real(-4))ac))(/1.02.0))))"
]