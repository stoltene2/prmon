{-# LANGUAGE RecordWildCards #-}
module Bitbucket
  ( Config(..)
  , Auth(..)
  , Handle
  , new
  , getOpenPullRequests
  ) where

import           Control.Lens ((^.), (.~), (?~), (&))
import           Data.Maybe (fromMaybe)
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Network.Wreq as Wreq

import qualified Logger
import qualified Bitbucket.Api as Api

------------------------------------------------------------------------------
data Config = Config
  { credentials :: Maybe Auth
  } deriving (Show)


data Auth = NoAuth
          | BasicAuth { username :: !Text , password :: !Text }
          | Token { token :: !Text }
          deriving (Show, Eq)


data Handle = Handle
  { logger :: Logger.Handle
  , creds  :: Auth
  }


new :: Config -> Logger.Handle -> Handle
new Config{..} h = Handle
  { logger = h
  , creds = fromMaybe NoAuth credentials
  }


getOpenPullRequests :: Handle -> String -> IO (Api.Page Api.PullRequest)
getOpenPullRequests Handle{..} uri = do
  let opts = case creds of
        NoAuth ->
          Wreq.defaults & Wreq.header "Accept" .~ ["application/json"]
        BasicAuth username password ->
          Wreq.defaults & Wreq.header "Accept" .~ ["application/json"]
                        & Wreq.auth ?~ Wreq.basicAuth (encodeUtf8 username) (encodeUtf8 password)
        Token token ->
          Wreq.defaults & Wreq.header "Accept" .~ ["application/json"]
                        & Wreq.auth ?~ Wreq.oauth2Bearer (encodeUtf8 token)

  r <- Wreq.asJSON =<< Wreq.getWith opts uri
  return (r ^. Wreq.responseBody)


{-
Using with GHCI
import           Logger
loggerH <- Logger.new (Logger.Config Nothing Nothing)
bbH <- new (Logger.Config (BasicAuth "username" "password")) loggerH
let resp = getOpenPullRequests bbH "https://<server>/rest/api/1.0/projects/<prefix>/repos/<repo-name>/pull-requests"
-}
