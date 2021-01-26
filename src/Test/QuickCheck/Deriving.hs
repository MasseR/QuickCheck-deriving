{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.QuickCheck.Deriving
  ( Isomorphic(..) )
  where

import GHC.Generics

import Data.Coerce

import Test.QuickCheck

newtype Isomorphic a b = Isomorphic b


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
