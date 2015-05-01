{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}

import Control.Applicative
import Control.Exception (throwIO)
import Data.Configifier
import Data.String.Conversions
import Data.Typeable
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Verbosity
import System.Environment
import System.Directory
import Text.Show.Pretty

import qualified Data.ByteString as SBS


type Config = Tagged (NoDesc ConfigDesc)
type ConfigDesc = ToConfigCode Config'

type Config' =
      ("cabalFile"  :> ST)
  :*> ("freezeFile" :> ST)

getConfig :: IO Config
getConfig = do
  let configFile :: FilePath = "./conf.yml"
  configFileSource :: [IO Source] <- do
    yes <- doesFileExist configFile
    if yes
        then (:[]) . return . ConfigFileYaml <$> SBS.readFile configFile
        else return []

  sources <- sequence $
    configFileSource ++
    [ ShellEnv       <$> getEnvironment
    , CommandLine    <$> getArgs
    ]

  either (throwIO) (return) (configify sources)


main :: IO ()
main = do
  config :: Config <- getConfig
  let cabalFile  :: FilePath = cs $ config >>. (Proxy :: Proxy '["cabalFile"])
      freezeFile :: FilePath = cs $ config >>. (Proxy :: Proxy '["freezeFile"])

  cabalFileContents <- readPackageDescription deafening cabalFile
  print cabalFileContents

--  freezeFileContents :: Int <- _ deafening freezeFile
--  print freezeFileContents


-- lowerBoundsFromFreezeFile :: FilePath -> FilePath -> IO ()
-- lowerBoundsFromFreezeFile = _
