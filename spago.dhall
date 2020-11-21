{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "datetime"
  , "effect"
  , "filterable"
  , "js-date"
  , "lenient-html-parser"
  , "node-http"
  , "node-process"
  , "node-streams"
  , "now"
  , "psci-support"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
