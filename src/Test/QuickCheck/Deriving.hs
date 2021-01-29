{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Text.QuickCheck.Deriving
Description : Arbitrary instance for isomorphic types
Copyright   : (c) Mats Rauhala, 2020
License     : BSD-3
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

Arbitrary instance for values that are isomorphic.

@
data Person
  = Person { name :: String
           , age :: Int
           }
  deriving stock (Show, Eq, Generic)
  deriving Arbitrary via ((PrintableString, Positive Int) `Isomorphic` Person)
@
-}
module Test.QuickCheck.Deriving
  ( Isomorphic(..)
  )
  where

import GHC.Generics

import Data.Coerce

import Test.QuickCheck

-- | Two values are isomorphic when @a `Isomorphic` b@
newtype Isomorphic a b = Isomorphic b

-- | Two types are coercible if their 'Generic' representations are coercible
type GenericCoercible a b =
  ( Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  )

genericCoerce :: forall a b. (GenericCoercible a b) => a -> b
genericCoerce = to . (coerce @(Rep a ()) @(Rep b ())) . from

instance (GenericCoercible a b, Arbitrary a) => Arbitrary (Isomorphic a b) where
  arbitrary = Isomorphic . genericCoerce @a @b <$> arbitrary
  shrink (Isomorphic b) =
    Isomorphic . genericCoerce @a @b <$> shrink (genericCoerce @b @a b)
