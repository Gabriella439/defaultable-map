{-# LANGUAGE QuantifiedConstraints #-}

{-| This module exports an API that is similar to "Defaultable.Map", except
    the utilities have been generalized further to work with any
    `Data.Map.Map`-like type.

    The only utility that cannot be generalized in this way is
    `Defaultable.Map.lookup`, so that is the only function missing from this
    module.  Other than the missing `lookup` function, this module is a drop-in
    replacement for the "Defaultable.Map" module.

    Also, keep in mind that these generalized utilities may have worse type
    inference (especially you omit type annotations) and in some cases might
    also be more inefficient.  If this is an issue for you then you'll need to
    create your own local module specializing these utilities to your
    `Data.Map.Map`-like type of interest.
-}
module Defaultable.Map.Generalized
    ( Defaultable(..)

      -- * Construction
    , fromMap
    , singleton
    , fromList
    , insert
    , withDefault

      -- * Query
    , toMap
    , toDefault
    ) where

import Control.Applicative (empty, (<|>))
import Data.Functor.Apply (Apply)
import Defaultable.Map (Defaultable(..))
import GHC.Exts (IsList(Item))

import qualified GHC.Exts as Exts

-- | Generalized version of `Defaultable.Map.fromMap`
fromMap :: map value -> Defaultable map value
fromMap map_ = Defaultable map_ empty

-- | Generalized version of `Defaultable.Map.singleton`
singleton :: IsList (map value) => Item (map value) -> Defaultable map value
singleton item = fromList [ item ]

-- | Generalized version of `Defaultable.Map.fromList`
fromList :: IsList (map value) => [ Item (map value) ] -> Defaultable map value
fromList items = fromMap (Exts.fromList items)

-- | Generalized version of `Defaultable.Map.insert`
insert
    :: (IsList (map value), Apply map, forall a . Monoid (map a))
    => Item (map value)
    -- ^
    -> Defaultable map value
    -- ^
    -> Defaultable map value
insert item defaultable = defaultable <|> singleton item

-- | Generalized version of `Defaultable.Map.withDefault`
withDefault
    :: (Apply map, forall a . Monoid (map a))
    => Defaultable map value
    -- ^
    -> value
    -- ^
    -> Defaultable map value
defaultable `withDefault` default_ = defaultable <|> pure default_

-- | Generalized version of `Defaultable.Map.toMap`
toMap :: Defaultable map value -> map value
toMap (Defaultable map_ _) = map_

-- | Generalized version of `Defaultable.Map.toDefault`
toDefault :: Defaultable map value -> Maybe value
toDefault (Defaultable _ default_) = default_
