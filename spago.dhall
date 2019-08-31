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
    , "prelude"
    , "psci-support"
    , "maybe"
    , "tuples"
    , "profunctor-lenses"
    , "group"
    , "generics-rep"
    , "foreign-generic"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}