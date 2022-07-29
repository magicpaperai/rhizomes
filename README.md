# ribbons

Implementation of `Ribbon` and `RibbonSet` types in Haskell.

A `Ribbon` consists of an origin and destination `Interval`, where `Intervals`
have a basis, a start integer, and an end integer. The basis is any type `a`
deriving `Eq`. A `RibbonSet` consists of a list of `Ribbons`.

In this project a `monoid` is defined on the type `RibbonSet`.
