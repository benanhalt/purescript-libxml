{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
	"my-project"
, dependencies =
	[ "arrays"
	, "effect"
	, "console"
	, "exceptions"
	, "nullable"
	, "foreign"
	, "generics-rep"
	, "psci-support"
	, "test-unit"
	]
, packages =
	./packages.dhall
, sources =
	[ "src/**/*.purs", "test/**/*.purs" ]
}
