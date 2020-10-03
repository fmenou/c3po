module C3PO.Utils
  ( listPlaceHolders
  ) where

import           C3PO.Types (MessageRepresentation (..), PlaceHolder)

listPlaceHolders :: MessageRepresentation -> [PlaceHolder]
listPlaceHolders (LiteralRepr     _) = mempty
listPlaceHolders (PlaceHolderRepr p) = pure p
listPlaceHolders (p1 :<> p2)         = listPlaceHolders p1
                                    <> listPlaceHolders p2
