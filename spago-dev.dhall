-- Spago configuration for testing, benchmarking, development.
--
-- See:
-- * ./CONTRIBUTING.md
-- * https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--

let conf = ./spago.dhall

let packages_dev = ./packages.dhall
		-- We're going to have to wait for spec to upgrade to purs 0.15
    with spec =
      { repo = "https://github.com/purescript-spec/purescript-spec.git"
      , version = "v5.0.0"
      , dependencies =
        [ "avar"
        , "console"
        , "aff"
        , "exceptions"
        , "strings"
        , "prelude"
        , "transformers"
        , "foldable-traversable"
        , "pipes"
        , "ansi"
        , "fork"
        , "now"
        ]
      }
in

conf //
{ sources = [ "src/**/*.purs", "test/**/*.purs", ]
, dependencies = conf.dependencies #
  [ "spec"
  , "psci-support"
  ]
, packages = packages_dev
}
