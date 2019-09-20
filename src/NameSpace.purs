module Libxml.NameSpace ( prefix , href) where

import Libxml.Types
import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import _prefix :: NameSpace -> Nullable String
foreign import href :: NameSpace -> String

prefix :: NameSpace -> Maybe String
prefix ns = toMaybe $ _prefix ns
