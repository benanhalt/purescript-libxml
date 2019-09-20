module Libxml.Document
       ( DocEncodingAndVersion
       , defaultDocEncodingAndVersion
       , newDoc
       , docGetRoot
       , docSetRoot
       , docCreateRoot
       , docFind
       , docFindWithNS
       , docGetElement
       , docGetAttr
       , docEncoding
       , docVersion
       , docToString
       , docSetEncoding
       , docChildNodes
       , docGetDtd
       )
where

import Libxml.Types
import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Libxml.Element (elementFind, elementFindWithNS, elementGetAttr, elementGetElement)


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
-- foreign import docFind :: String -> Document -> Effect (Array Element)
foreign import _docNode :: EffectFn3 String String Document Element
foreign import _docRoot :: EffectFn1 Document (Nullable Element)
foreign import _docSetRoot :: forall a. EffectFn2 (Node a) Document Element
foreign import _docGetDtd :: EffectFn1 Document (Nullable DTD)

newDoc :: DocEncodingAndVersion -> Effect Document
newDoc options = runEffectFn2 _newDoc options.version options.encoding

docCreateRoot :: String -> String -> Document -> Effect Element
docCreateRoot name content document = runEffectFn3 _docNode name content document

docGetRoot :: Document -> Effect (Maybe Element)
docGetRoot document = toMaybe <$> runEffectFn1 _docRoot document

docSetRoot :: forall a. Node a -> Document -> Effect Unit
docSetRoot root document = void $ runEffectFn2 _docSetRoot root document

docGetDtd :: Document -> Effect (Maybe DTD)
docGetDtd document = toMaybe <$> runEffectFn1 _docGetDtd document

docGetElement :: String -> Document -> Effect (Maybe Element)
docGetElement xpath doc = docGetRoot doc >>= case _ of
  Nothing -> pure Nothing
  Just root -> elementGetElement xpath root

docGetAttr :: String -> Document -> Effect (Maybe Attribute)
docGetAttr xpath doc = docGetRoot doc >>= case _ of
  Nothing -> pure Nothing
  Just root -> elementGetAttr xpath root

docFind :: String -> Document -> Effect (Maybe XPathResult)
docFind xpath doc = docGetRoot doc >>= case _ of
  Nothing -> pure Nothing
  Just root -> elementFind xpath root

docFindWithNS :: String -> String -> Document -> Effect (Maybe XPathResult)
docFindWithNS xpath href doc = docGetRoot doc >>= case _ of
  Nothing -> pure Nothing
  Just root -> elementFindWithNS xpath href root
