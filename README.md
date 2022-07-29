# ribbons

Implementation of a `Ribbon` type in Haskell.

A `Ribbon` consists of an origin and destination `Interval`, where `Intervals`
have a basis, a start integer, and an end integer. The basis is any type `a`
deriving `Eq`.

The primary purpose of this project was to implement the following function.

```haskell
compose :: [Ribbon a] -> [Ribbon a] -> [Ribbon a]
```
