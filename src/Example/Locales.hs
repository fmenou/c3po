{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Example.Locales
  ( MyLocale(..)
  , locales
  , localesFromFile
  ) where

import           C3PO (ToLocaleCode (..), deriveLocales)

data MyLocale = EnglishUK
              | FrenchFrance

instance ToLocaleCode MyLocale where
  localeCode EnglishUK    = "en_UK"
  localeCode FrenchFrance = "fr_FR"

deriveLocales ''MyLocale
