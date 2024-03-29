{ name = "int64"
, dependencies =
  [ "prelude"
  , "integers"
  , "effect"
  , "foreign"
  , "nullable"
  , "functions"
  , "maybe"
  , "gen"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/purescript-contrib/purescript-int64"
}
