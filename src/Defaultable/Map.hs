{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module provides a `Defaultable` type constructor for extending
-- `Map`-like types with a valid `Applicative` and `Alternative` instance.  If
--  you're looking for an \"`Applicative` `Map`\" then you are in the right
--  place!
--
--  The `Defaultable` type constructor can be used to wrap any `Map`-like, such
--  as @"Data.Map".`Data.Map.Map`@ or @"Data.HashMap".`Data.HashMap.HashMap`@.
--
--  For convenience, this module also includes a concrete API wrapping
--  @"Data.Map".`Data.Map.Map`@ since that's the most common case.  If you
--  are interested in a more general API that works with other `Map` types then
--  check out the "Defaultable.Map.Generalized" module.
--
--  The `Applicative` instance enables the use of the @ApplicativeDo@ language
--  extension.  For example, suppose that you created the following three
--  `Defaultable` `Map`s:
--
-- @
-- firstNames, lastNames, handles :: `Defaultable` (`Map` `Int`) `String`
-- firstNames = `fromList` [(0, \"Gabriella\"    ), (1, \"Oscar\"), (2, \"Edgar\"    )                  ]
-- lastNames  = `fromList` [(0, \"Gonzalez\"     ),               (2, \"Codd\"     ), (3, \"Bryant\"   )]
-- handles    = `fromList` [(0, \"GabriellaG439\"), (1, \"posco\"),                   (3, \"avibryant\")]
-- @
--
--  Then you can use @ApplicativeDo@ notation to create an \"inner join\" of
--  these various maps, like this:
--
-- >>> :set -XApplicativeDo
-- >>> do firstName <- firstNames; lastName <- lastNames; return (firstName, lastName)
-- Defaultable (fromList [(0,("Gabriella","Gonzalez")),(2,("Edgar","Codd"))]) Nothing
--
-- … and you can join as many of these maps as you want by adding statements
-- to these @ApplicativeDo@ blocks:
--
-- @
-- {-# LANGUAGE ApplicativeDo #-}
--
-- innerJoins :: `Defaultable` (`Map` Int) (`String`, `String`, `String`)
-- innerJoins = do
--     firstName <- firstNames
--     lastName  <- lastNames
--     handles   <- handles
--     return (firstName, lastName, handles)
-- @
--
-- >>> innerJoins
-- Defaultable (fromList [(0,("Gabriella","Gonzalez","GabriellaG439"))]) Nothing
--
--  The `Alternative` instance for `Defaultable` is also important, too, because
--  you can use `Alternative` operations to create \"left/right joins\" and
--  something similar to an outer join, like this:
--
-- @
-- leftJoin :: `Defaultable` (`Map` `Int`) (`String`, `Maybe` `String`)
-- leftJoin = do
--     firstName <- firstNames
--     lastName  <- `Control.Applicative.optional` lastNames
--     return (firstName, lastName)
--
-- rightJoin :: `Defaultable` (`Map` `Int`) (`Maybe` `String`, `String`)
-- rightJoin = do
--     firstName <- `Control.Applicative.optional` firstNames
--     lastName  <- lastNames
--     return (firstName, lastName)
--
-- 
-- similarToOuterJoin :: `Defaultable` (`Map` `Int`) (`Maybe` `String`, `Maybe` `String`)
-- similarToOuterJoin = do
--     firstName <- `Control.Applicative.optional` firstNames
--     lastName  <- `Control.Applicative.optional` lastNames
--     return (firstName, lastName)
-- @
--
-- >>> leftJoin
-- Defaultable (fromList [(0,("Gabriella",Just "Gonzalez")),(1,("Oscar",Nothing)),(2,("Edgar",Just "Codd"))]) Nothing
-- >>> rightJoin
-- Defaultable (fromList [(0,(Just "Gabriella","Gonzalez")),(2,(Just "Edgar","Codd")),(3,(Nothing,"Bryant"))]) Nothing
-- >>> similarToOuterJoin
-- Defaultable (fromList [(0,(Just "Gabriella",Just "Gonzalez")),(1,(Just "Oscar",Nothing)),(2,(Just "Edgar",Just "Codd")),(3,(Nothing,Just "Bryant"))]) (Just (Nothing,Nothing))
--
-- You can also do more interesting multiway joins where any combination of
-- the inputs may be `Control.Applicative.optional`:
-- 
-- @
-- complexJoin :: `Defaultable` (`Map` `Int`) (`Maybe` `String`, `String`, `Maybe` `String`)
-- complexJoin = do
--     firstName <- `Control.Applicative.optional` firstNames
--     lastName  <- lastNames
--     handle    <- `Control.Applicative.optional` handles
--     return (firstName, lastName, handle)
-- @
--
-- >>> complexJoin
-- Defaultable (fromList [(0,(Just "Gabriella","Gonzalez",Just "GabriellaG439")),(2,(Just "Edgar","Codd",Nothing)),(3,(Nothing,"Bryant",Just "avibryant"))]) Nothing

module Defaultable.Map
    ( -- * Comparison
      -- $comparison

      -- * Type
      Defaultable(..)
    , Map

      -- * Construction
    , fromMap
    , singleton
    , fromList
    , insert
    , withDefault

      -- * Query
    , lookup
    , toMap
    , toDefault
    ) where

import Control.Applicative (liftA2, Alternative(..))
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Functor.Alt (Alt(..))
import Data.Functor.Apply (Apply(..))
import Data.Map (Map)
import GHC.Generics (Generic, Generic1)
import Prelude hiding (lookup)

import qualified Data.Map as Map

-- $comparison
--
-- This package is similar to the
-- <https://hackage.haskell.org/package/total-map total-map package>,
-- which also provides an \"`Applicative` `Map`\" type.  However, there are a
-- couple of differences.
--
-- The first difference is that this package does not require you to supply a
-- default value in order to get a valid `Applicative` instance.  In other
-- words the default value is optional.  In contrast, the @total-map@ package
-- requires you to supply a default value.  That means that the `lookup`
-- function from this package can return `Nothing`, whereas the analogous
-- @(!)@ operator from the @total-map@ package always returns a value.
--
-- However, the benefit of this tradeoff is that this package can provide an
-- `Alternative` instance for `Defaultable`, whereas the @total-map@ package
-- does not have a valid `Alternative` instance.  Furthermore, the `Alternative`
-- instance enables support for left\/right\/\"outer\" joins as noted above.
--
-- Also, sometimes you just need an `Applicative` `Map` without a default value.
--
-- The other key difference compared to @total-map@ is that this package works
-- with `Map`-like types other than @"Data.Map".`Data.Map.Map`@, whereas
-- @total-map@ is hard-coded to @"Data.Map".`Data.Map.Map`@.  The only caveat
-- is that if you use the `Defaultable` type to wrap other `Map`-like types
-- (such as @"Data.HashMap".`Data.HashMap.HashMap`@) then you need to create
-- your own utility functions, such as a new `lookup` function for a
-- `Defaultable` `Data.HashMap.HashMap`.  However, this is not hard to do, as
-- you'll see if you consult the source code for each utility function.

{-| A `Defaultable` type is a @Map@-like type that is extended with an optional
    default value.  This default value can be used as a fallback if a lookup
    into the @Map@-like type does not find a matching key.

    The type variables are:

    * @map@: The @Map@-like type to wrap (typically including the type of key,
      but not the type of the value)

    * @value@ The type of each value stored in the @Map@-like type

    For example, you will typically have a type like
    @`Defaultable` (`Map` key) value@ or
    @`Defaultable` `Data.IntMap.IntMap` value@.

    You can build a `Defaultable` value using:

    * `empty` - The empty `Defaultable` has no keys and no default value
    * `pure` - A `Defaultable` with a default value but no keys
    * `fromMap` \/ `fromList` \/ `singleton` - Convenient construction functions
    * The `Defaultable` constructor

    You can transform and combine `Defaultable` values using:

    * (`<|>`) - Concatenate two `Defaultable` values, preferring keys and
      defaults from the left one
    * @do@ notation, if you enable @ApplicativeDo@
    * `withDefault` - To extend a `Defaultable` value with a default value

    You can query a `Defaultable` value using:

    * `lookup`
    * `toMap` / `toDefault`

    Note that the `Applicative` instance for this type is only valid for
    @map@ type constructors that satisfy the following extra law:

@
Given:

• mf :: map (a -> b)
• mx :: map a
• kf :: (a -> b) -> c
• kx :: a -> c

  (mf `<.>` mx) `<>` `fmap` kf mf `<>` `fmap` kx mx
= (mf `<.>` mx) `<>` `fmap` kx mx `<>` `fmap` kf mf
@

    … where `map` is the first type parameter that implements `Apply` and
    `Monoid`.

    The intuition here is if that @map@ is a `Map`-like type then we can think
    of those three expressions as having a set of keys associated with them,
    such that:

@
Given:

• keys :: map a -> `Data.Set.Set` key

keys (mf `<.>` mx) = keys (`fmap` kf mf) \`intersection\` keys (`fmap` kx mx)
@

    So normally the following equality would not be true:

@
  `fmap` kf mf `<>` `fmap` kx mx
= `fmap` kx mx `<>` `fmap` kf mf
@

    … because the result would change if there was a key collision.  Then the
    order in which we union (`<>`) the two maps would change the result.

    However, if you union yet another map (@mf `<.>` mx@) that shadows the
    colliding keys then result remains the same.
-}
data Defaultable map value =
    Defaultable
        (map value)
        -- ^ The underlying @Map@-like type
        (Maybe value)
        -- ^ An optional default value to return if a key is missing
    deriving stock
        ( Data
        , Eq
        , Foldable
        , Functor
        , Generic
        , Generic1
        , Ord
        , Show
        , Traversable
        )
    deriving anyclass (NFData)

instance (Apply map, forall a . Semigroup (map a)) => Apply (Defaultable map) where
    Defaultable fMap Nothing <.> Defaultable xMap Nothing =
        Defaultable (fMap <.> xMap) Nothing
    Defaultable fMap (Just f) <.> Defaultable xMap Nothing =
        Defaultable ((fMap <.> xMap) <> fmap f xMap) Nothing
    Defaultable fMap Nothing <.> Defaultable xMap (Just x) =
        Defaultable ((fMap <.> xMap) <> fmap ($ x) fMap) Nothing
    Defaultable fMap (Just f) <.> Defaultable xMap (Just x) =
        Defaultable ((fMap <.> xMap) <> fmap f xMap <> fmap ($ x) fMap) (Just (f x))

instance (Apply map, forall a . Monoid (map a)) => Applicative (Defaultable map) where
    pure v = Defaultable mempty (pure v)

    Defaultable fMap fDefault <*> Defaultable xMap xDefault =
        Defaultable fxMap fxDefault
      where
        fxMap = (fMap <.> xMap) <> fFallback <> xFallback
          where
            fFallback =
                case fDefault of
                    Nothing -> mempty
                    Just f  -> fmap f xMap

            xFallback =
                case xDefault of
                    Nothing -> mempty
                    Just x  -> fmap ($ x) fMap

        fxDefault = fDefault <*> xDefault

instance (Apply map, forall a . Semigroup (map a)) => Alt (Defaultable map) where
    Defaultable lMap lDefault <!> Defaultable rMap rDefault =
        Defaultable (lMap <> rMap) (lDefault <|> rDefault)

instance (Apply map, forall a . Monoid (map a)) => Alternative (Defaultable map) where
    empty = Defaultable mempty empty

    Defaultable lMap lDefault <|> Defaultable rMap rDefault =
        Defaultable (lMap <> rMap) (lDefault <|> rDefault)

-- | Not the same as the `Semigroup` instance for the underlying @map@ type
instance (Apply map, forall a . Monoid (map a), Semigroup value) => Semigroup (Defaultable map value) where
    (<>) = liftA2 (<>)

-- | Not the same as the `Monoid` instance for the underlying @map@ type
instance (Apply map, forall a . Monoid (map a), Monoid value) => Monoid (Defaultable map value) where
    mempty = pure mempty

{-| Create a `Defaultable` `Map` from a `Map`

>>> fromMap (Map.fromList [('A',1),('B',2),('B',3)])
Defaultable (fromList [('A',1),('B',3)]) Nothing
-}
fromMap :: Map key value -> Defaultable (Map key) value
fromMap map_ = Defaultable map_ empty

{-| Create a `Defaultable` `Map` from a single key-value pair

>>> singleton ('A', 1)
Defaultable (fromList [('A',1)]) Nothing
-}
singleton :: (key, value) -> Defaultable (Map key) value
singleton (key, value) = fromMap (Map.singleton key value)

{-| Create a `Defaultable` `Map` from a list of key-value pairs

>>> fromList [('A',1),('B',2),('B',3)]
Defaultable (fromList [('A',1),('B',3)]) Nothing
-}
fromList :: Ord key => [(key, value)] -> Defaultable (Map key) value
fromList pairs = fromMap (Map.fromList pairs)

{-| Insert a key-value pair into a `Defaultable` `Map`

>>> let example = fromList [('A', 1)]
>>> insert ('B', 2) example
Defaultable (fromList [('A',1),('B',2)]) Nothing
>>> insert ('A', 0) example
Defaultable (fromList [('A',0)]) Nothing

    For bulk updates, you should instead use `fromList`/`fromMap` with (`<|>`):

>>> fromList [('A',0),('B', 2), ('C', 3)] <|> example
Defaultable (fromList [('A',0),('B',2),('C',3)]) Nothing
-}
insert
    :: Ord key
    => (key, value)
    -- ^
    -> Defaultable (Map key) value
    -- ^
    -> Defaultable (Map key) value
insert (key, value) (Defaultable map_ default_) =
    (Defaultable (Map.insert key value map_) default_)

{-| Add a default value to a `Defaultable` `Map` that is returned as a fallback
    if a `lookup` cannot find a matching key

>>> let example = fromList [('A',1)] `withDefault` 2
>>> lookup 'A' example
Just 1
>>> lookup 'B' example
Just 2
-}
withDefault
    :: Ord key
    => Defaultable (Map key) value
    -- ^
    -> value
    -- ^
    -> Defaultable (Map key) value
Defaultable map_ _ `withDefault` default_ = Defaultable map_ (Just default_)

{-| Lookup the value at a key in the map

    If the key is missing this falls back to returning the default value if
    present

    `lookup` is an `Monad` morphism, meaning that `lookup` distributes
    over `Monad` operatiorns:

@
`lookup` (`return` x) = `return` x

`lookup` (do x <- m; f x) = do x <- `lookup` m; `lookup` (f x)
@

    `lookup` is also an `Alternative` morphism, meaning that `lookup`
    distributes over `Alternative` operations:

@
`lookup` `empty` = `empty`

`lookup` (l `<|>` r) = `lookup` l `<|>` `lookup` r
@

>>> let example = fromList [('A',1)]
>>> lookup 'A' example
Just 1
>>> lookup 'B' example
Nothing
>>> lookup 'B' (example `withDefault` 2)
Just 2
-}
lookup :: Ord key => key -> Defaultable (Map key) value -> Maybe value
lookup key (Defaultable map_ default_) = Map.lookup key map_ <|> default_

-- | Extract the underlying map from a `Defaultable` map
toMap :: Defaultable (Map key) value -> Map key value
toMap (Defaultable map_ _) = map_

-- | Extract the default value from a `Defaultable` map
toDefault :: Defaultable (Map key) value -> Maybe value
toDefault (Defaultable _ default_) = default_
