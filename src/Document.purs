module Libxml.Document
       ( DocEncodingAndVersion
       , defaultDocEncodingAndVersion
       , newDoc
       , docGetRoot
       , docCreateRoot
       , docFind
       , docEncoding
       , docVersion
       , docToString
       , docSetEncoding
       , docChildNodes
       , docGetDtd
       )
where

import Prelude (Unit, void, ($), (<$>))
import Libxml.Types


import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)


type DocEncodingAndVersion = {encoding :: String, version :: String}

defaultDocEncodingAndVersion :: DocEncodingAndVersion
defaultDocEncodingAndVersion = {encoding: "utf8", version: "1.0"}

foreign import _newDoc :: EffectFn2 String String Document
foreign import docChildNodes :: Document -> Effect (Array (Node Unit))
foreign import docEncoding :: Document -> Effect String
foreign import docVersion :: Document -> Effect String
foreign import docSetEncoding :: String -> Document -> Effect Unit
foreign import docToString :: Document -> Effect String
foreign import docValidate :: Document -> Document -> Effect Unit
foreign import docFind :: String -> Document -> Effect (Array Element)
foreign import _docNode :: EffectFn3 String String Document Element
foreign import _docRoot :: EffectFn1 Document (Nullable Element)
foreign import _docSetRoot :: EffectFn2 Element Document Element
foreign import _docGetDtd :: EffectFn1 Document (Nullable DTD)

newDoc :: DocEncodingAndVersion -> Effect Document
newDoc options = runEffectFn2 _newDoc options.version options.encoding

docCreateRoot :: String -> String -> Document -> Effect Element
docCreateRoot name content document = runEffectFn3 _docNode name content document

docGetRoot :: Document -> Effect (Maybe Element)
docGetRoot document = toMaybe <$> runEffectFn1 _docRoot document

docSetRoot :: Element -> Document -> Effect Unit
docSetRoot root document = void $ runEffectFn2 _docSetRoot root document

docGetDtd :: Document -> Effect (Maybe DTD)
docGetDtd document = toMaybe <$> runEffectFn1 _docGetDtd document

