module Main where

import Data.Maybe
import System.IO
import System.Environment

import Network.URI

import Parse
import Fetch

crawl :: URI -> IO ()
crawl rootURL = do
  pageContent <- fetchUrl rootURL
  let images = mapMaybe (`relativeTo` rootURL) $ imageLinks pageContent
  mapM_ downloadUrl images

main = do
  args <- getArgs
  let validURIs = mapMaybe parseURI args
  mapM_ crawl validURIs
