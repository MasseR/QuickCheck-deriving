{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
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
  , ASCIIText(..)
  , Corpus(..)
  , Range(..)
  , UTCRange(..)
  -- * Lenses
  , _PrintableText
  , _ASCIIText
  , _Corpus
  , _Range
  , _DayRange
  , _UTCRange
  )
  where

import Control.Lens
       (Iso', from, iso, view)
import Data.Text.Strict.Lens
       (packed)

import Data.Proxy
       (Proxy(..))
import GHC.TypeLits
       (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

import Test.QuickCheck

import Data.Text
       (Text)
import qualified Data.Text as T

import Data.Time
       (Day(..), UTCTime(..))
import Data.Time.Calendar
       (addDays, diffDays, fromGregorian)

shrinkIso :: Arbitrary a => Iso' a b -> b -> [b]
shrinkIso i = shrinkMap (view i) (view (from i))

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

-- | Lens for accessing 'PrintableText'
_PrintableText :: Iso' Text PrintableText
_PrintableText = iso PrintableText getPrintableText

instance Arbitrary PrintableText where
  arbitrary = PrintableText . T.pack <$> fmap getPrintableString arbitrary
  shrink = shrinkIso (packed . _PrintableText)

-- | Modifier to return ascii 'Text'
--
-- Same as 'ASCIIString' but for 'Text' instead
--
-- @
-- data Person = Person { name :: Text, age :: Int }
--  deriving stock (Show, Eq, Generic)
--  deriving (Arbitrary) via ((ASCIIText, Int) `Isomorphic` Person)
-- @
newtype ASCIIText = ASCIIText { getASCIIText :: Text }
  deriving (Show, Eq)

-- | Lens for accessing 'ASCIIText'
_ASCIIText :: Iso' Text ASCIIText
_ASCIIText = iso ASCIIText getASCIIText

instance Arbitrary ASCIIText where
  arbitrary = ASCIIText . T.pack <$> fmap getASCIIString arbitrary
  shrink = shrinkIso (packed . _ASCIIText)

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

-- | Lens for accessing 'Corpus'
_Corpus :: Iso' Text (Corpus corpus)
_Corpus = iso Corpus getCorpus

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

-- | Lens for accessing 'Range'
_Range :: Iso' x (Range a b x)
_Range = iso Range getRange

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
  shrink (Range x) = Range . (+ start) <$> shrinkIntegral (x - start)
    where
      start = fromIntegral (natVal (Proxy @a))

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

_DayRange :: Iso' Day (DayRange from to)
_DayRange = iso DayRange getDayRange

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
  arbitrary = DayRange . (`addDays` start) <$> choose (0, diff)
    where
      diff = diffDays end start
      start = fromGregorian (natVal (Proxy @y)) (fromIntegral (natVal (Proxy @m))) (fromIntegral (natVal (Proxy @d)))
      end = fromGregorian (natVal (Proxy @y')) (fromIntegral (natVal (Proxy @m'))) (fromIntegral (natVal (Proxy @d')))
  shrink (DayRange x) = DayRange <$> shrinkMap (`addDays` start) (`diffDays` start) x
    where
      start = fromGregorian (natVal (Proxy @y)) (fromIntegral (natVal (Proxy @m))) (fromIntegral (natVal (Proxy @d)))

-- | Modifier to return a random time in inclusive range
--
-- Return a random 'UTCTime' value that is within the inclusive range of 'from' to 'to'
--
-- @
-- data Person = Person { name :: Text, born :: Day }
--  deriving stock (Show, Eq, Generic)
--  deriving (Arbitrary) via ((Text, UTCTime (1980 10 23 0) (2000 01 01 300)) `Isomorphic` Person)
-- @
newtype UTCRange (from :: (Nat, Nat, Nat, Nat)) (to :: (Nat, Nat, Nat, Nat)) = UTCRange { getUTCRange :: UTCTime }
  deriving (Show, Eq)

_UTCRange :: Iso' UTCTime (UTCRange from to)
_UTCRange = iso UTCRange getUTCRange

instance
  ( KnownNat y
  , KnownNat m
  , KnownNat d
  , KnownNat s
  , KnownNat y'
  , KnownNat m'
  , KnownNat d'
  , KnownNat s'
  , '(y,m,d,s) ~ from
  , '(y',m',d',s') ~ to
  ) => Arbitrary (UTCRange from to) where
  arbitrary = UTCRange <$> (UTCTime <$> genDay <*> genTime)
    where
      genDay = fmap (getDayRange @'(y,m,d) @'(y',m',d')) arbitrary
      genTime = fmap (fromInteger . getRange @s @s') arbitrary
  shrink (UTCRange (UTCTime d t)) =
    UTCRange <$>
      [UTCTime d' t | DayRange d' <- shrink (DayRange @'(y,m,d) @'(y',m',d') d)] ++
      [UTCTime d (fromInteger t') | Range t' <- shrink (Range @s @s' (floor t))] ++
      [ UTCTime d' (fromInteger t')
      | DayRange d' <- shrink (DayRange @'(y,m,d) @'(y',m',d') d)
      , Range t' <- shrink (Range @s @s' (floor t))
      ]
