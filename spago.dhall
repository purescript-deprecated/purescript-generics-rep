{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "generics-rep"
, dependencies =
  [ "console", "effect", "psci-support", "tuples", "enums", "assert" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
