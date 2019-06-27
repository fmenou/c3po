{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Example.Locales where

import           Data.Map            (Map)
import           Data.String         (IsString)
import           Language.Haskell.TH (Name)

data MyLocale = FrenchFrance | EnglishUK

allLocales :: (IsString str, Ord str) => Map str Name
allLocales = [ ("fr_FR" , 'FrenchFrance)
             , ("en_UK" , 'EnglishUK   )
             ]
