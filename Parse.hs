{-# LANGUAGE OverloadedStrings #-}

module Parse (imageLinks) where

import Data.List (lookup)
import Data.Maybe (mapMaybe)

import Network.URI

import qualified Data.ByteString.Lazy.Char8 as B
import Text.HTML.TagSoup

-- | If the tag has this name, Just this tag, else Nothing.
tagNamed :: B.ByteString -> Tag B.ByteString -> Maybe (Tag B.ByteString)
tagNamed name t@(TagOpen tagName _) = if name == tagName 
    then Just t 
    else Nothing
tagNamed _ _ = Nothing

-- | If the tag has attributes, Just the attributes, otherwise Nothing.
tagAttributes :: Tag B.ByteString -> Maybe [Attribute B.ByteString]
tagAttributes (TagOpen _ attributes) = Just attributes
tagAttributes _                      = Nothing

-- | Just the 'src' attribute of an <img> tag, or Nothing
imageSource :: Tag B.ByteString -> Maybe B.ByteString
imageSource tag = do
  imageTag <- tagNamed "img" tag
  attrs    <- tagAttributes imageTag
  lookup "src" attrs

-- | Ferret out the URIs in all the image tags in the supplied document.
imageLinks :: B.ByteString -> [URI]
imageLinks = makeUris . mapMaybe imageSource . parseTags
  where
    makeUris :: [B.ByteString] -> [URI]
    makeUris = mapMaybe parseURIReference . map B.unpack

