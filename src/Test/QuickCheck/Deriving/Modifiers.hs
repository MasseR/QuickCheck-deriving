{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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

-- | Modifier to return a random element in inclusive range
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
    where
      shrunk = [ x'
               | x' <- shrinkIntegral x
               , x' >= fromIntegral (natVal (Proxy @a))
               , x' <= fromIntegral (natVal (Proxy @b))
               ]
