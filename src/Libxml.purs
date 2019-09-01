module Libxml
       ( libxmljs_version
       , libxml_version
       , libxml_parser_version
       , memoryUsage
       , nodeCount
       , parseXmlString
       , parseHtmlString
       , parseHtmlFragment
       )
where

import Prelude (($))
import Libxml.Types

import Data.Either (Either)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import libxmljs_version :: String
foreign import libxml_version :: String
foreign import libxml_parser_version :: String


foreign import _parseXmlString :: EffectFn1 String Document
foreign import _parseHtmlString :: EffectFn1 String Document
foreign import _parseHtmlFragment :: EffectFn1 String Document

parseXmlString :: String -> Effect (Either Error Document)
parseXmlString string = try $ runEffectFn1 _parseXmlString string

parseHtmlString :: String -> Effect (Either Error Document)
parseHtmlString string = try $ runEffectFn1 _parseHtmlString string

parseHtmlFragment :: String -> Effect (Either Error Document)
parseHtmlFragment string = try $ runEffectFn1 _parseHtmlFragment string

foreign import memoryUsage :: Effect Int
foreign import nodeCount :: Effect Int
