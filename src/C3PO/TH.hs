{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeOperators      #-}

module C3PO.TH
  ( implementation
  , deriveLocalesDefinition
  ) where

import           C3PO.Types
import           C3PO.Utils               (listPlaceHolders)

import           Control.Monad.Reader     (MonadReader, ReaderT, asks, lift,
                                           local, runReaderT)
import           Data.List                (sortOn)
import           Data.Map                 as M (Map, assocs, fromList, (!))
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Formatting               as F (sformat, stext, (%))
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift ()
import           Named                    hiding (Name)

class ReifiedLocale l where
  reifyLocale :: l -> Name

enumerateLocales :: (ReifiedLocale l, ToLocaleCode l, Enum l, Bounded l) => proxy l -> Map LocaleCode Name
enumerateLocales proxy =
  let toPair = (,) <$> localeCode <*> reifyLocale
  in fromList (toPair <$> enumerate proxy)

enumerate :: (Enum l, Bounded l) => proxy l -> [l]
enumerate _ = enumFromTo minBound maxBound

deriveLocalesDefinition :: Name -> Q [Dec]
deriveLocalesDefinition ty = do
  TyConI tyCon <- reify ty
  cons <- case tyCon of
    DataD    _ _ _ _ cs _ -> pure cs
    NewtypeD _ _ _ _ cs _ -> pure [cs]
    _ -> fail "deriveLocalesDefinition: tyCon may not be a type synonym."
  (:) <$> instanceD (pure []) [t| ReifiedLocale $(conT ty) |] [mkInstances cons]
      <*> prerequisites ty

deriveInstanceIfMissing :: Name -> Name -> Q [Dec]
deriveInstanceIfMissing tc ty = do
  alreadyThere <- isInstance tc [ConT ty]
  if alreadyThere
  then pure mempty
  else deriveInstance tc ty

deriveInstance :: Name -> Name -> Q [Dec]
deriveInstance tc ty =
  [d| deriving instance $(conT tc) $(conT ty) |]

prerequisites :: Name -> Q [Dec]
prerequisites ty =
  mconcat <$> (
    sequence
      [ deriveInstanceIfMissing ''Enum    ty
      , deriveInstanceIfMissing ''Bounded ty
      , [d| localesDefinition :: Locales
            localesDefinition = Locales ($([e| ty |]), enumerateLocales (Proxy :: Proxy $(conT ty)))
        |]
      ]
  )

mkInstances :: [Con] -> Q Dec
mkInstances = funD 'reifyLocale . fmap mkInstance

mkInstance :: Con -> Q Clause
mkInstance (NormalC n _) =
  let pats = [conP n []]
      body = normalB [e| n |]
  in clause pats body []
mkInstance _ = fail "deriveLocalesDefinition: unsupported constructor"

implementation :: Name -> Map LocaleCode Name -> Messages -> Q [Dec]
implementation localeType localeCodes (Messages msgs) =
  let placeholders = mempty
  in runReaderT (mconcat <$> traverse implementMessage (assocs msgs)) TEnv{..}

type T = ReaderT TEnv Q

data TEnv = TEnv { localeType   :: Name
                 , localeCodes      :: Map LocaleCode Name
                 , placeholders :: Map PlaceHolder Name
                 }

getLocaleName :: LocaleCode -> T Name
getLocaleName l = do
  ls <- asks localeCodes
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
