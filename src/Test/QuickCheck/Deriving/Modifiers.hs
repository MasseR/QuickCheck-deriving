{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Text.QuickCheck.Deriving.Modifiers
Description : Modifiers to QuickCheck generation
Copyright   : (c) Mats Rauhala, 2020
License     : BSD-3
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

Modifiers for QuickCheck generation
-}
module Test.QuickCheck.Deriving.Modifiers
  ( PrintableText(..)
  , Corpus(..)
  , Range(..)
  , DayRange(..)
  )
  where

import Data.Proxy
       (Proxy(..))
import GHC.TypeLits
       (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

import Test.QuickCheck

import Data.Text
       (Text)
import qualified Data.Text as T

import Data.Time
       (Day(..))
import Data.Time.Calendar
       (addDays, diffDays, fromGregorian)

-- Still finding the design space for this

-- | Modifier to return printable 'Text'
--
-- Same as 'PrintableString' but for 'Text' instead
--
-- @
-- data Person = Person { name :: Text, age :: Int }
--  deriving stock (Show, Eq, Generic)
--  deriving (Arbitrary) via ((PrintableText, Int) `Isomorphic` Person)
-- @
newtype PrintableText = PrintableText { getPrintableText :: Text }
  deriving (Show, Eq)

instance Arbitrary PrintableText where
  arbitrary = PrintableText . T.pack . getPrintableString <$> arbitrary
  shrink (PrintableText t) = PrintableText . T.pack . getPrintableString <$> shrink (PrintableString (T.unpack t))

-- | Modifier to return a random element of a text corpus
--
-- Returns a random element of a text corpus. The corpus is given as a type argument.
--
-- @
-- type MetaSyntactic = '["foo", "bar", "baz", "xyzzy"]
-- data Person = Person { name :: Text, age :: Int }
--  deriving stock (Show, Eq, Generic)
--  deriving (Arbitrary) via ((Corpus MetaSyntactic, Int) `Isomorphic` Person)
-- @
newtype Corpus (corpus :: [Symbol]) = Corpus { getCorpus :: Text }
  deriving (Show, Eq)

-- | A class for converting a type level list of symbols into a value level list of text
class FromCorpus (a :: [Symbol]) where
  fromCorpus :: [Text]

instance FromCorpus '[] where
  fromCorpus = []

instance (KnownSymbol x, FromCorpus xs) => FromCorpus (x ': xs) where
  fromCorpus = T.pack (symbolVal (Proxy @x)) : fromCorpus @xs

instance (FromCorpus corpus) => Arbitrary (Corpus corpus) where
  arbitrary = Corpus <$> elements (fromCorpus @corpus)
  shrink (Corpus x) = Corpus <$> takeWhile (/= x) (fromCorpus @corpus)

newtype Range (a :: Nat) (b :: Nat) x = Range { getRange :: x }
  deriving (Show, Eq)

-- | Modifier to return a random number in inclusive range
--
-- Return a random 'Integral' value that is within the inclusive range of a to b
--
-- @
-- data Person = Person { name :: Text, age :: Int }
--  deriving stock (Show, Eq, Generic)
--  deriving (Arbitrary) via ((Text, Range 0 100 Int) `Isomorphic` Person)
-- @
instance (KnownNat a, KnownNat b, Integral x, Num x) => Arbitrary (Range a b x) where
  arbitrary = Range . fromIntegral <$> choose (natVal (Proxy @a), natVal (Proxy @b))
  shrink (Range x) = Range . (+ from) <$> shrinkIntegral (x - from)
    where
      from = fromIntegral (natVal (Proxy @a))

-- | Modifier to return a random date in inclusive range
--
-- Return a random 'Date' value that is within the inclusive range of 'from' to 'to'
--
-- @
-- data Person = Person { name :: Text, born :: Day }
--  deriving stock (Show, Eq, Generic)
--  deriving (Arbitrary) via ((Text, DateRange (1980 10 23) (2000 01 01)) `Isomorphic` Person)
-- @
newtype DayRange (from :: (Nat, Nat, Nat)) (to :: (Nat, Nat, Nat)) = DayRange { getDayRange :: Day }
  deriving (Show, Eq)

instance
  ( KnownNat y
  , KnownNat m
  , KnownNat d
  , KnownNat y'
  , KnownNat m'
  , KnownNat d'
  , '(y,m,d) ~ from
  , '(y',m',d') ~ to
  ) => Arbitrary (DayRange from to) where
  arbitrary = DayRange . (`addDays` from) <$> choose (0, diff)
    where
      diff = diffDays to from
      from = fromGregorian (natVal (Proxy @y)) (fromIntegral (natVal (Proxy @m))) (fromIntegral (natVal (Proxy @d)))
      to = fromGregorian (natVal (Proxy @y')) (fromIntegral (natVal (Proxy @m'))) (fromIntegral (natVal (Proxy @d')))
  shrink (DayRange x) = DayRange <$> shrinkMap (`addDays` from) (`diffDays` from) x
    where
      from = fromGregorian (natVal (Proxy @y)) (fromIntegral (natVal (Proxy @m))) (fromIntegral (natVal (Proxy @d)))
