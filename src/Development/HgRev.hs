{-# LANGUAGE OverloadedStrings #-}

module Development.HgRev
       ( hgRevState
       , hgRev
       , hgState
       , hgShortRev
       , hgIsDirty
       , HgRev (..)
       , HgState (..)
       ) where

import           Control.Applicative        ((<$>), (<*>))
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


-- | Get the hg revision and working directory state for a given repo.
hgRevState :: FilePath -- ^ Path anywhere within the repository
           -> IO (Maybe (HgRev, HgState)) -- ^ Nothing is returned if no repo or `hg` binary are found
hgRevState repo = do
    rev <- hgRev repo
    state <- hgState repo
    return $ (,) <$> rev <*> state


-- | Get the hg revision for a given repo.
hgRev :: FilePath -- ^ Path anywhere within the repository
      -> IO (Maybe HgRev) -- ^ Nothing is returned if no repo or `hg` binary are found
hgRev repo = join . fmap parse <$> runHg repo args
      where
        args = ["log", "-r.", "-Tjson", "--config='defaults.log='"]
        parse = join . fmap listToMaybe . decode' . pack


-- hg does not yet have a programmatic way to get dirty state of
-- working dir so this separate call is needed.
-- | Get the hg working directory state for a given repo.
hgState :: FilePath -- ^ Path anywhere within the repository
        -> IO (Maybe HgState) -- ^ Nothing is returned if no repo or `hg` binary are found
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
  { hgRevision  :: String -- ^ Universally unique revision hash
  , hgBranch    :: String -- ^ Branch name
  , hgTags      :: [String] -- ^ Tags
  , hgBookmarks :: [String] -- ^ Bookmarks
  } deriving (Show, Eq)


data HgState
    = Clean -- ^ No uncommitted changes in working directory
    | Dirty -- ^ Uncommitted changes exist in working directory
    deriving (Show, Eq)


instance FromJSON HgRev where
    parseJSON (Object x) =
        HgRev
        <$> x .: "node"
        <*> x .: "branch"
        <*> x .: "tags"
        <*> x .: "bookmarks"
    parseJSON invalid = typeMismatch "HgRev" invalid


-- | Get the hg short revision which is the first 12 hex characters of the hash.
hgShortRev :: HgRev -> String
hgShortRev = take 12 . hgRevision


-- | Bool indication of dirty working directory state.
hgIsDirty :: HgState -> Bool
hgIsDirty Dirty = True
hgIsDirty Clean = False
