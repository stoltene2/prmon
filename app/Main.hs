{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Lens
import           Control.Monad (forM_)
import qualified Data.List
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO (putStrLn)
import qualified Data.Yaml as Yaml
import           Data.Yaml ((.:))
import           GHC.Generics (Generic)
import           System.Directory (getXdgDirectory, XdgDirectory(..), doesFileExist)
import           System.FilePath ((</>))

import qualified Bitbucket
import qualified Bitbucket.Api as Api
import qualified Logger

data Config = Config
  { auth :: Bitbucket.Auth
  , endpoint :: Text
  } deriving (Show, Generic)

instance Yaml.FromJSON Config where
  parseJSON = Yaml.withObject "Config" $ \o ->
    Config <$> (Bitbucket.Token <$> (o .: "authToken"))
           <*> o .: "endpoint"


main :: IO ()
main = do
  let loggerConfig = Logger.Config Nothing Nothing

  Logger.withHandle loggerConfig $ \loggerHandle -> do
    econfig <- loadUserConfig
    case econfig of
      (Right Config{..}) -> do
        let bitBucketH = Bitbucket.new (Bitbucket.Config (Just auth)) loggerHandle

        results <- Bitbucket.getOpenPullRequests bitBucketH (unpack endpoint)

        let sorted = Data.List.sortOn (\a -> a ^. Api.toRef ^. Api.displayId) (results ^. Api.values)
        let grouped = foldl' insertIntoMap Map.empty sorted

        forM_ (Map.toList grouped) $ \(branch, prs) -> do
          let nameLen = Text.length branch
          TIO.putStrLn branch
          TIO.putStrLn (Text.replicate nameLen "=")
          forM_ prs $ \pr ->
            TIO.putStrLn
              (pr ^. Api.id .to show .to pack <> " " <> pr ^. Api.title)
          TIO.putStrLn ""

        Logger.info loggerHandle (results ^.Api.values .to Prelude.length .to show ++ " open PRs")

      Left err -> Logger.error loggerHandle err

  where
    insertIntoMap result pr =  Map.insertWith (++) (pr ^. Api.toRef ^. Api.displayId) [pr] result

loadUserConfig :: IO (Either Text Config)
loadUserConfig = do
  configFile <- getXdgDirectory XdgConfig "prmon" >>= \path -> return (path </> "config.yaml")
  fileExists <- doesFileExist configFile

  if fileExists
    then do
      econfig <- Yaml.decodeFileEither configFile
      case econfig of
        Left e -> return . Left . pack . show $ e
        Right config -> return $ Right config
    else return . Left $
         "Configuration file could not be found: " <> pack configFile
