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
import Control.Lens
import Control.Monad (void, forM)
import Data.Char (toLower)
import Data.Configifier
import Data.Function (on)
import Data.Functor.Infix ((<$$>))
import Data.List (intercalate, sortBy, isPrefixOf)
import Data.Maybe (catMaybes)
import Data.String.Conversions
import Data.Typeable
import Data.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Verbosity
import Distribution.Version
import Network.Wreq hiding (Proxy)
import System.Directory
import System.Environment
import Text.Show.Pretty

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Attoparsec.Combinator as AP
import qualified Data.ByteString as SBS
import qualified Data.Map.Strict as Map


type Config = Tagged ConfigDesc
type ConfigDesc = ToConfigCode Config'

type Config' =
      Maybe ("cabalFile"   :> ST)
  :*>       ("freezeFiles" :> [ST])

getConfig :: IO Config
getConfig = do
  let configFile :: FilePath = "./conf.yml"
  configFileSource :: [Source] <- do
    yes <- doesFileExist configFile
    return $ if yes
      then [YamlFile configFile]
      else []

  sources <- (configFileSource ++) <$> (sequence
    [ ShellEnv       <$> getEnvironment
    , CommandLine    <$> getArgs
    ])

  configify sources


main' :: IO ()
main' = do
  config :: Config <- getConfig
  let Just cabalFile :: Maybe FilePath = cs <$> config >>. (Proxy :: Proxy '["cabalFile"])
      freezeFiles :: [FilePath] = cs <$> config >>. (Proxy :: Proxy '["freezeFiles"])

  cabalFileContents <- readPackageDescription deafening cabalFile
  print cabalFileContents

  -- the cabal config file parser lives in cabal-install in
  -- Distribution.Client.Config, which is not exposed in a library.
  -- so what this was all about won't work as simply as i hoped...

--  freezeFileContents :: Int <- _ deafening freezeFile
--  print freezeFileContents


-- lowerBoundsFromFreezeFile :: FilePath -> FilePath -> IO ()
-- lowerBoundsFromFreezeFile = _


----------------------------------------------------------------------

-- pull stackage config, relax version constraints, and impose on package file's `build-depends`
-- section.

main :: IO ()
main = do
  config :: Config <- getConfig
  let freezeFiles :: [FilePath] = cs <$> config >>. (Proxy :: Proxy '["freezeFiles"])

  versionFreeze <- mconcat . reverse <$> forM freezeFiles (\ freezeFile -> do
    putStrLn $ "  -- source: " ++ freezeFile

    versionFreezeStr :: SBS
      <- if "http://" `isPrefixOf` freezeFile || "https://" `isPrefixOf` freezeFile
           then cs . (^. responseBody) <$> get freezeFile
           else SBS.readFile freezeFile

    let Right versionFreezePart = parseFreezeFile versionFreezeStr
    -- print versionFreezePart
    -- print $ Map.size versionFreezePart
    return versionFreezePart)

  Right (deps :: [ST]) <- parseBuildDependsBlob . cs <$> getContents
  putStrLn . showConstraints $ injectConstraints deps versionFreeze


type VersionFreeze = Map.Map ST Version

-- | Since cabal-install doesn't export cabal config file parsing as a library, we re-implement the
-- parts we need for stackage here.
parseFreezeFile :: SBS -> Either String VersionFreeze
parseFreezeFile = AP.eitherResult . AP.parse p
  where
    p :: AP.Parser VersionFreeze
    p = do
      AP.many' comment
      AP.string "constraints:"
      Map.fromList . catMaybes <$> AP.many' constraint

    comment :: AP.Parser ()
    comment = AP.string "--" >> AP.manyTill' AP.anyChar end >> return ()

    end :: AP.Parser ()
    end = AP.endOfLine <|> AP.endOfInput

    constraint :: AP.Parser (Maybe (ST, Version))
    constraint = do
      AP.many' AP.space
      n :: String <- AP.manyTill' AP.anyChar AP.space
      AP.many' AP.space
      ((do AP.string "=="
           v :: Version <- version []
           end
           return $ Just (cs n, v)) <|>
       (do AP.string "installed,"
           return Nothing))

    version :: [Int] -> AP.Parser Version
    version acc = do
      i :: Int <- AP.decimal
      let acc' = acc ++ [i]
      ((AP.char ',' >> return (Version acc' [])) <|>
       (AP.char '.' >> version acc'))


-- | Read a `build-depends` cut&pasted from a package description file (discard version
-- constraints).
parseBuildDependsBlob :: SBS -> Either String [ST]
parseBuildDependsBlob s = AP.eitherResult $ AP.feed (AP.parse p s) ""
  where
    p :: AP.Parser [ST]
    p = AP.many' space' >> AP.sepBy' (cs <$> it) sep

    sep :: AP.Parser ()
    sep = AP.manyTill' AP.anyChar (AP.char ',') >> void (AP.many' space')

    space' :: AP.Parser ()
    space' = void AP.space <|> AP.endOfLine

    it :: AP.Parser String
    it = (:) <$> AP.satisfy (AP.inClass "a-zA-Z") <*> AP.many' (AP.satisfy (AP.inClass "-0-9a-zA-Z"))


type VersionConstraint = Either Version VersionInterval
type VersionConstraints = Map.Map ST VersionConstraint

injectConstraints :: [ST] -> VersionFreeze -> VersionConstraints
injectConstraints packages freeze = Map.fromList $ f <$> packages
  where
    f :: ST -> (ST, VersionConstraint)
    f package = (package,) $ case Map.lookup package freeze of
        Just v -> mkConstraint v
        Nothing -> error $ "injectConstraints: package " <> show package <> " not found in freeze files."

mkConstraint :: Version -> VersionConstraint
mkConstraint (Version v@(a:b:_) _) = Right
    ( LowerBound (Version v []) InclusiveBound
    , UpperBound (Version [a, b+1] []) ExclusiveBound
    )
mkConstraint v = Left v

showConstraints :: VersionConstraints -> String
showConstraints constraints = "      " <> intercalate "\n    , " (f <$> sort' (Map.toList constraints))
  where
    f :: (ST, VersionConstraint) -> String
    -- f (p, (Left (Version [] _))) = cs p
    f (p, (Left v)) = cs p <> " ==" <> showVersion v
    f (p, (Right (LowerBound l InclusiveBound, UpperBound u ExclusiveBound))) =
        cs p <> " >=" <> showVersion l <> " && <" <> showVersion u

    sort' :: [(ST, a)] -> [(ST, a)]
    sort' = sortBy (compare `on` (map toLower . cs . fst))
