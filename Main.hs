module Main where

import Control.Applicative ((<$>))
import Data.Maybe
import System.FilePath
import System.IO
import System.Environment

import Control.Concurrent.ParallelIO
import Network.URI

import Parse
import Fetch

-- | Download all the images linked to in the document at this URI.
crawl :: URI -> IO ()
crawl rootURL = do
  -- fetch the page
  putStrLn $ "processing " ++ show rootURL
  pageContent <- fetchUrl rootURL
  
  -- get all the image links as URIs
  let images = mapMaybe (`relativeTo` rootURL) $ imageLinks pageContent
  
  -- fetch each image
  parallel_ $ map fetchImage images
    where
      fetchImage image = do
        putStrLn $ "  fetching " ++ show image
        downloadUrl image

-- | Treat each command line argument as a top-level URI and download the 
-- images therefrom.
main = do
  validURIs <- mapMaybe parseURI <$> getArgs
  mapM_ crawl validURIs
  stopGlobalPool
