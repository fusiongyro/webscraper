module Parse (imageLinks) where

import Data.List (lookup)
import Data.Maybe (mapMaybe)

import Network.URI
import Text.HTML.TagSoup

tagNamed :: String -> Tag String -> Maybe (Tag String)
tagNamed name t@(TagOpen tagName _) = if name == tagName 
    then Just t 
    else Nothing
tagNamed name _ = Nothing

tagAttributes :: Tag String -> Maybe [Attribute String]
tagAttributes (TagOpen _ attributes) = Just attributes
tagAttributes _                      = Nothing

imageSource :: Tag String -> Maybe String
imageSource tag = do
  imageTag <- tagNamed "img" tag
  attrs <- tagAttributes imageTag
  lookup "src" attrs

imageLinks :: String -> [URI]
imageLinks = mapMaybe parseURIReference . process . parseTags
  where
    process :: [Tag String] -> [String]
    process = mapMaybe imageSource
