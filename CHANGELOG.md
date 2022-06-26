1.0.2

* BUG: Fix `Applicative` instance for `Defaultable`
  * The original instance violated the composition/associativity law for
    `Applicative`s

1.0.1

* BUG FIX: `Defaultable.Map.Generalized.insert` now correctly overrides any
  existing key
* Small fixes to documentation about how key collisions are resolved

1.0.0

* Initial release
