BeginPackage["ChristopherWolfram`Z3Link`LibZ3`"];

Begin["`Private`"];

Needs["ChristopherWolfram`Z3Link`"]


$LibZ3 := $LibZ3 =
	Switch[$SystemID,
		"Linux-x86-64",  "/home/linuxbrew/.linuxbrew/Cellar/z3/4.12.1/lib/libz3.so.4.12.1.0",
		"MacOSX-x86-64", "/usr/local/Cellar/z3/4.12.1/lib/libz3.4.12.1.0.dylib",
		"MacOSX-ARM64",  "/opt/homebrew/Cellar/z3/4.12.2/lib/libz3.dylib"
	]


End[];
EndPackage[];
