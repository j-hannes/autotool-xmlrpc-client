{-# LANGUAGE OverloadedStrings #-}

module Autotool.Utils.XmlParser
  ( getNodeContent
  , parseDocumentation
  , fixSpecialChars
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Either.Unwrap (isLeft, fromRight)
import           Data.String.Utils (replace)
import qualified Data.Text as T
import qualified Text.XmlHtml as X


-----------------------------------------------------------------------------
-- | Get a plain text version from a XML document.
getNodeContent :: String -> String
getNodeContent input =
    fixSpecialChars . concatTextNodes $ reduceTags tags xml
  where
    tags           = ["Pre", "Beside", "Above"]
    xml            = X.docContent xmldoc
    (Right xmldoc) = X.parseXML "" (BS.pack input)


-----------------------------------------------------------------------------
-- | Replace special characters in a string.
--   TODO: Complete charMap!
fixSpecialChars :: String -> String
fixSpecialChars word = foldl (\s (a,b) -> replace a b s) word charMap
  where
    charMap = [
        ("&#223;", "ß")
      , ("&#228;", "ä")
      , ("&#246;", "ö")
      , ("&#252;", "ü")
      , ("&apos;", "'")
      , ("&quot;", "\"")
      ]


-----------------------------------------------------------------------------
-- | Reduce a XML forest by a list of tags.
reduceTags :: [String] -> [X.Node] -> [X.Node]
reduceTags tags xml = foldl reduceTag xml tags


-----------------------------------------------------------------------------
-- | Reduce a XML forest by one tag. The children of the tag are put in place
-- of the tag itself.
reduceTag :: [X.Node] -> String -> [X.Node]
reduceTag [] _ = []
reduceTag (node:nodes) tag
    | X.isElement node && tagMatches = reduce ++ next
    | X.isElement node               = reduceChildren : next
    | otherwise                      = node           : next      
  where
    tagMatches     = X.elementTag node == T.pack tag
    reduce         = reduceTag (X.elementChildren node) tag
    reduceChildren = node {X.elementChildren = reduce}
    next           = reduceTag nodes tag
    

-----------------------------------------------------------------------------
-- | Concatenate the text of nodes and separate them by a blank line.
concatTextNodes :: [X.Node] -> String
concatTextNodes = concatMap (\x -> T.unpack (X.nodeText x) ++ "\n\n")
 

-----------------------------------------------------------------------------
-- | Convert a XML string into a list of tuples.
parseDocumentation :: String -> [(String, String)]
parseDocumentation input =
    if isLeft parsed
      then []
      else let xmlDoc = fromRight parsed
               firstChild = head (X.docContent xmlDoc)
           in  if X.elementTag firstChild == "Link"
                 then [collectLink firstChild]
                 else map collectLink $
                        filter (\x -> X.elementTag x == T.pack "Link") $
                        X.childNodes firstChild
  where
    parsed = X.parseXML "" (BS.pack input)


-----------------------------------------------------------------------------
-- | Parse a specific XML node structure into a tuple.
collectLink :: X.Node -> (String, String)
collectLink (X.Element "Link" [(_, link)] [(X.TextNode linkText)]) =
    (T.unpack linkText, T.unpack link)
collectLink _ = error "could not parse link"
