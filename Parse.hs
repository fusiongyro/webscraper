module Parse (imageLinks) where

import Data.List (lookup)
import Data.Maybe (mapMaybe)

import Network.URI
import Text.HTML.TagSoup

-- | If the tag has this name, Just this tag, else Nothing.
tagNamed :: String -> Tag String -> Maybe (Tag String)
tagNamed name t@(TagOpen tagName _) = if name == tagName 
    then Just t 
    else Nothing
tagNamed name _ = Nothing

-- | If the tag has attributes, Just the attributes, otherwise Nothing.
tagAttributes :: Tag String -> Maybe [Attribute String]
tagAttributes (TagOpen _ attributes) = Just attributes
tagAttributes _                      = Nothing

-- | Just the 'src' attribute of an <img> tag, or Nothing
imageSource :: Tag String -> Maybe String
imageSource tag = do
  imageTag <- tagNamed "img" tag
  attrs <- tagAttributes imageTag
  lookup "src" attrs

-- | Ferret out the URIs in all the image tags in the supplied document.
imageLinks :: String -> [URI]
imageLinks = mapMaybe parseURIReference . process . parseTags
  where
    process :: [Tag String] -> [String]
    process = mapMaybe imageSource
