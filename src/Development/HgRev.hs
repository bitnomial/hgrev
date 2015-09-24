{-# LANGUAGE OverloadedStrings #-}

module Development.HgRev
       ( hgRevState
       , hgRev
       , hgState
       , HgRev (..)
       , HgState (..)
       ) where

import           Control.Monad              (join)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), decode', object, (.:),
                                             (.=))
import           Data.Aeson.Types           (typeMismatch)
import           Data.Bool                  (bool)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.List                  (isInfixOf)
import           Data.Maybe                 (listToMaybe)
import           System.Exit                (ExitCode (..))
import           System.FilePath            (FilePath)
import           System.Process             (readProcessWithExitCode)


hgRevState :: FilePath -> IO (Maybe (HgRev, HgState))
hgRevState repo = do
    rev <- hgRev repo
    state <- hgState repo
    return $ (,) <$> rev <*> state


hgRev :: FilePath -> IO (Maybe HgRev)
hgRev repo = do
    (ec, stdout, _) <- readProcessWithExitCode hg args stdin
    return . join . maybeExitCode ec $ parse stdout
      where
        hg = "hg"
        args = ["log", "-r.", "-Tjson", "--config='defaults.log='", "-R", repo]
        stdin = ""
        parse = join . fmap listToMaybe . decode' . pack


hgState :: FilePath -> IO (Maybe HgState)
hgState repo = do
    (ec, stdout, _) <- readProcessWithExitCode hg args stdin
    return . maybeExitCode ec $ bool Clean Dirty $ "+" `isInfixOf` stdout
      where
        hg = "hg"
        args = ["identify", "--config='defaults.identify=-i'", "-R", repo]
        stdin = ""


maybeExitCode :: ExitCode -> a -> Maybe a
maybeExitCode ExitSuccess x = Just x
maybeExitCode (ExitFailure _) _ = Nothing


data HgRev =
  HgRev
  { hgRevision  :: String
  , hgBranch    :: String
  , hgTags      :: [String]
  , hgBookmarks :: [String]
  } deriving (Show, Eq)


data HgState = Clean | Dirty deriving (Show, Eq)

instance FromJSON HgRev where
    parseJSON (Object x) =
        HgRev
        <$> x .: "node"
        <*> x .: "branch"
        <*> x .: "tags"
        <*> x .: "bookmarks"
    parseJSON invalid = typeMismatch "HgRev" invalid

instance ToJSON HgRev where
    toJSON x =
        object
        [ "revision"  .= hgRevision x
        , "branch"    .= hgBranch x
        , "tags"      .= hgTags x
        , "bookmarks" .= hgBookmarks x
        ]
