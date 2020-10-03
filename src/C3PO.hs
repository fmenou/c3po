{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module C3PO
  ( deriveLocales
  , ToLocaleCode(..)
  ) where

import           C3PO.Parser
import           C3PO.TH                   (deriveLocalesDefinition,
                                            implementation)
import           C3PO.Utils                (listPlaceHolders)

import           Data.List                 (nub, sort)
import           Data.Map                  as M (insert, lookup)
import           Data.Maybe                (fromMaybe)
import           Language.Haskell.TH       (Dec, Name, Q)
import           Language.Haskell.TH.Quote (QuasiQuoter (..), quoteFile)

deriveLocales :: Name -> Q [Dec]
deriveLocales n =
  mconcat <$>
    sequence [ deriveLocalesDefinition n
             , quasiQuoters
             ]

quasiQuoters :: Q [Dec]
quasiQuoters =
  [d|
    locales :: QuasiQuoter
    locales =
      let unsupported t = fail ("locales: quasiquoter '" <> t <> "' is not supported")
      in QuasiQuoter { quoteDec  = c3po localesDefinition
                     , quoteExp  = unsupported "e"
                     , quotePat  = unsupported "p"
                     , quoteType = unsupported "t"
                     }
    localesFromFile :: QuasiQuoter
    localesFromFile = quoteFile locales
  |]

c3po :: Locales -> String -> Q [Dec]
c3po (Locales (localeType, locales)) = either fail (implementation localeType locales . compileMessages) . parseLocales

compileMessages :: LocalesDefinitions -> Messages
compileMessages (LocalesDefinitions defs) = foldl compile mempty defs

compile :: Messages -> MessageDefinition -> Messages
compile (Messages msgs) MessageDefinition{..} =
  let implementations = addImplementation (fromMaybe mempty (M.lookup defMessageId msgs))
      addImplementation (Message placeholders mapping) = Message (sort (nub (placeholders <> listPlaceHolders defRepresentation))) (M.insert defLocaleCode defRepresentation mapping)
  in Messages (M.insert defMessageId implementations msgs)
