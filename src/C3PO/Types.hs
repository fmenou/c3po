{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module C3PO.Types
  ( LocalesDefinitions(..)
  , MessageDefinition(..)
  , MessageId(..)
  , LocaleCode(..)
  , ToLocaleCode(..)
  , Locales(..)
  , PlaceHolder(..)
  , MessageRepresentation(..)
  , Messages(..)
  , Message(..)
  ) where

import           Data.Map            (Map)
import           Data.String         (IsString)
import           Language.Haskell.TH (Name)

newtype LocalesDefinitions = LocalesDefinitions [MessageDefinition] deriving stock (Show)

data MessageDefinition =
  MessageDefinition { defMessageId      :: MessageId
                    , defLocaleCode     :: LocaleCode
                    , defRepresentation :: MessageRepresentation
                    } deriving Show

newtype MessageId   = MessageId   String deriving stock (Show) deriving newtype (Eq, Ord, IsString)
newtype LocaleCode  = LocaleCode  String deriving stock (Show) deriving newtype (Eq, Ord, IsString)
newtype PlaceHolder = PlaceHolder String deriving stock (Show) deriving newtype (Eq, Ord, IsString)

data MessageRepresentation = LiteralRepr String
                           | PlaceHolderRepr PlaceHolder
                           | MessageRepresentation :<> MessageRepresentation
                             deriving Show

instance Semigroup MessageRepresentation where
  (<>) = (:<>)

newtype Messages = Messages (Map MessageId Message)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

data Message = Message [PlaceHolder] (Map LocaleCode MessageRepresentation)
  deriving stock (Show)

instance Semigroup Message where
  Message ps1 m1 <> Message ps2 m2 = Message (ps1 <> ps2) (m1 <> m2)

instance Monoid Message where
  mempty = Message mempty mempty

class ToLocaleCode l where
  localeCode :: l -> LocaleCode

newtype Locales = Locales (Name, Map LocaleCode Name)
