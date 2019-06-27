{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module C3PO.TH
  ( implementation
  ) where

import           C3PO.Types
import           C3PO.Utils           (listPlaceHolders)

import           Control.Monad.Reader (MonadReader, ReaderT, asks, lift, local,
                                       runReaderT)
import           Data.List            (sortOn)
import           Data.Map             as M (Map, assocs, fromList, (!))
import           Data.Text            (Text)
import           Formatting           as F (sformat, stext, (%))
import           Language.Haskell.TH
import           Named                hiding (Name)

implementation :: Name -> Map LocaleCode Name -> Messages -> Q [Dec]
implementation localeType locales (Messages msgs) =
  let placeholders = mempty
  in runReaderT (mconcat <$> traverse implementMessage (assocs msgs)) TEnv{..}

type T = ReaderT TEnv Q

data TEnv = TEnv { localeType   :: Name
                 , locales      :: Map LocaleCode Name
                 , placeholders :: Map PlaceHolder Name
                 }

getLocaleName :: LocaleCode -> T Name
getLocaleName l = do
  ls <- asks locales
  pure (ls M.! l)

getLocaleType :: T TypeQ
getLocaleType = asks (conT . localeType)

formatLocalizeds :: Map LocaleCode MessageRepresentation -> T [Clause]
formatLocalizeds = traverse formatLocalized . M.assocs

formatLocalized :: (LocaleCode, MessageRepresentation) -> T Clause
formatLocalized (l, r) = do
  ln <- getLocaleName l
  let matchLocale = [p| $(conP ln []) |]
  body <- formattingExpression r
  lift $ clause [matchLocale] (normalB body) []

getPlaceholderName :: PlaceHolder -> T Name
getPlaceholderName ph =
  let query = flip (M.!)
  in asks (query ph . placeholders)

getPlaceholderReference :: PlaceHolder -> T ExpQ
getPlaceholderReference = fmap varE . getPlaceholderName

formattingExpression :: MessageRepresentation -> T ExpQ
formattingExpression (LiteralRepr lit) = pure (constantText lit)
formattingExpression (PlaceHolderRepr ph) = getPlaceholderReference ph
formattingExpression r =
  let mkBody values = foldl appE [e| F.sformat |] (formatter r : values)
      referencePlaceholders = traverse getPlaceholderReference . listPlaceHolders
  in mkBody <$> referencePlaceholders r

constantText :: String -> ExpQ
constantText = litE . stringL

formatter :: MessageRepresentation -> ExpQ
formatter (LiteralRepr lit)   = constantText lit
formatter (PlaceHolderRepr _) = [e| F.stext |]
formatter (repr1 :<> repr2)   = [e| $(formatter repr1) F.% $(formatter repr2) |]

mkPlaceHolderName :: PlaceHolder -> T (PlaceHolder, Name)
mkPlaceHolderName (PlaceHolder ph) = (PlaceHolder ph,) <$> lift (newName ph)

implementMessage :: (MessageId, Message) -> T [Dec]
implementMessage (MessageId messageId, Message phs rs) = do
  m <- lift (newName "m")
  functionName <- lift (newName messageId)
  subFunctionName <- lift (newName "msg")
  lt <- getLocaleType
  phs' <- fromList <$> traverse mkPlaceHolderName phs
  clauses <- withPlaceholderNames phs' (formatLocalizeds rs)
  let placeholdersMapping = sortOn fst (assocs phs')
      placeholders = fst <$> placeholdersMapping
      placeholderNames = snd <$> placeholdersMapping
      pats = mkPattern <$> placeholderNames
      types = placeholderType <$> placeholders
      textInM = [t| $(varT m) $text |]
      signature = foldr arrowQ textInM types
      constraint = [t| MonadReader $lt $(varT m) => $signature |]
      impl' = funD subFunctionName (pure <$> clauses)
      body = normalB (letE [impl'] [e| asks $(varE subFunctionName) |])
      definition = clause pats body []
  lift
    $ sequence [ sigD functionName constraint
               , funD functionName [definition]
               ]

withPlaceholderNames :: Map PlaceHolder Name -> T a -> T a
withPlaceholderNames phs = local (\e -> e { placeholders = phs })

mkPattern :: Name -> Q Pat
mkPattern n = [p| Arg $(varP n) |]

text :: TypeQ
text = [t| Text |]

arrowQ :: TypeQ -> TypeQ -> TypeQ
arrowQ a b = [t| $a -> $b |]

placeholderType :: PlaceHolder -> TypeQ
placeholderType (PlaceHolder ph) =
  let promote = litT . strTyLit
  in [t| $(promote ph) :! Text |] -- ^ This is using 'Named'
