{-# LANGUAGE RecordWildCards #-}

module C3PO
  ( c3po
  ) where


import           C3PO.Parser
import           C3PO.TH             (implementation)
import           C3PO.Utils          (listPlaceHolders)

import           Data.List           (nub, sort)
import           Data.Map            as M (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           Language.Haskell.TH (Dec, Name, Q)

c3po :: Name -> Map LocaleCode Name -> String -> Q [Dec]
c3po localeType locales = either fail (impl localeType locales) . parseLocales

impl :: Name -> Map LocaleCode Name -> LocalesDefinitions -> Q [Dec]
impl localeType locales = implementation localeType locales . compileMessages

compileMessages :: LocalesDefinitions -> Messages
compileMessages (LocalesDefinitions defs) = foldl compile mempty defs

compile :: Messages -> MessageDefinition -> Messages
compile (Messages msgs) MessageDefinition{..} =
  let implementations = addImplementation (fromMaybe mempty (M.lookup defMessageId msgs))
      addImplementation (Message placeholders mapping) = Message (sort (nub (placeholders <> listPlaceHolders defRepresentation))) (M.insert defLocaleCode defRepresentation mapping)
  in Messages (M.insert defMessageId implementations msgs)
