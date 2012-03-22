module Fetch (fetchUrl, downloadUrlTo, downloadUrl) where

import System.FilePath

import Network.HTTP (simpleHTTP, getResponseBody, getRequest)
import Network.URI

-- | Fetches the supplied URL and returns the string contents
fetchUrl :: URI -> IO String
fetchUrl url = do
  response <- simpleHTTP (getRequest (show url))
  getResponseBody response

-- | Downloads the supplied URL into the supplied filename
downloadUrlTo :: URI -> FilePath -> IO ()
downloadUrlTo url filename = fetchUrl url >>= writeFile filename

-- | Download the supplied URL to a local file with the basename of this URL
downloadUrl :: URI -> IO ()
downloadUrl url = downloadUrlTo url (takeFileName (uriPath url))
