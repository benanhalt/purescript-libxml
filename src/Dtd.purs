module Libxml.DTD (dtdName, dtdExternalId, dtdSystemId) where

import Prelude (($))
import Libxml.Types

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import dtdName :: DTD -> String
foreign import _dtdExternalId :: DTD -> Nullable String
foreign import _dtdSystemId :: DTD -> Nullable String

dtdExternalId :: DTD -> Maybe String
dtdExternalId dtd = toMaybe $ _dtdExternalId dtd

dtdSystemId :: DTD -> Maybe String
dtdSystemId dtd = toMaybe $ _dtdSystemId dtd
