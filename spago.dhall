{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "undoable"
, dependencies =
    [ "assert"
    , "console"
    , "effect"
    , "foreign-generic"
    , "generics-rep"
    , "group"
    , "maybe"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "run"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
