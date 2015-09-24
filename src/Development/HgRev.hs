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
import           System.Process             (cwd, proc,
                                             readCreateProcessWithExitCode)


hgRevState :: FilePath -> IO (Maybe (HgRev, HgState))
hgRevState repo = do
    rev <- hgRev repo
    state <- hgState repo
    return $ (,) <$> rev <*> state


hgRev :: FilePath -> IO (Maybe HgRev)
hgRev repo = join . fmap parse <$> runHg repo args
      where
        args = ["log", "-r.", "-Tjson", "--config='defaults.log='"]
        parse = join . fmap listToMaybe . decode' . pack


-- hg does not yet have a programmatic way to get dirty state of
-- working dir so this separate call is needed.
hgState :: FilePath -> IO (Maybe HgState)
hgState repo = (fmap . fmap) check $ runHg repo args
      where
        args = ["identify", "-i", "--config='defaults.identify='"]
        check = bool Clean Dirty . (isInfixOf "+")


runHg :: FilePath -> [String] -> IO (Maybe String)
runHg repo args = do
    (ec, stdout, _) <- readCreateProcessWithExitCode (setCwd repo $ proc "hg" args) ""
    return $ maybeExitCode ec stdout
  where
    maybeExitCode ExitSuccess x = Just x
    maybeExitCode (ExitFailure _) _ = Nothing
    setCwd y x = x{cwd= Just y}


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
