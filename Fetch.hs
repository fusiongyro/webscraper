{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchUrl, downloadUrlTo, downloadUrl) where

import Prelude hiding (catch)
import Control.Exception
import Control.Applicative ((<$>))
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP.Conduit
import Network.URI

-- | Fetches the supplied URL and returns the string contents
fetchUrl :: URI -> IO B.ByteString
fetchUrl url = catch (simpleHttp (show url)) errorHandler
  where
    errorHandler :: HttpException -> IO B.ByteString
    errorHandler _ = do
      putStrLn $ "Error downloading " ++ show url
      return ""

-- | Downloads the supplied URL into the supplied filename
downloadUrlTo :: URI -> FilePath -> IO ()
downloadUrlTo url filename = fetchUrl url >>= B.writeFile filename

-- | Download the supplied URL to a local file with the basename of this URL
downloadUrl :: URI -> IO ()
downloadUrl url = downloadUrlTo url $ takeFileName $ uriPath url