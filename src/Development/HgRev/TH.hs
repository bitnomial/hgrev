{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.HgRev.TH where

import           Data.Aeson                 (ToJSON (..), encode, object, (.=))
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Char                  (toLower)
import           Data.Monoid                ((<>))
import           Development.HgRev          (HgRev (..), HgState (..),
                                             hgIsClean, hgRevState, hgShortRev)
import           Language.Haskell.TH        (ExpQ, runIO, stringE)
import           System.Directory           (getCurrentDirectory)


-- | Function to format hg rev and state info for printing
type FormatFn = (HgRev -> HgState -> String)


-- | Apply a format function and return a Template Haskell spliced
-- string for compiling into code.
--
-- @since 0.2.5
hgRevStateTHDir :: FilePath -> FormatFn -> ExpQ
hgRevStateTHDir dir format = do
    revState <- runIO $ hgRevState dir
    stringE $ maybe "UNKNOWN" (uncurry format) revState


-- | Apply a format function and return a Template Haskell spliced
-- string for compiling into code.
hgRevStateTH :: FormatFn -> ExpQ
hgRevStateTH format = do
    revState <- runIO $ hgRevState =<< getCurrentDirectory
    stringE $ maybe "UNKNOWN" (uncurry format) revState


-- |
-- >  long:  d9d3a1172a1d919b3056b435891081c0d7d00599
-- >  short: d9d3a1172a1d
-- >  clean: true
defFormat :: FormatFn
defFormat rev state
    =  "\n  long:  " <> hgRevision rev
    <> "\n  short: " <> hgShortRev rev
    <> "\n  clean: " <> (map toLower . show $ hgIsClean state)


-- |
-- > {
-- >   "clean": true,
-- >   "short": "d9d3a1172a1d",
-- >   "branch": "default",
-- >   "bookmarks": [],
-- >   "long": "d9d3a1172a1d919b3056b435891081c0d7d00599",
-- >   "tags": [
-- >     "tip"
-- >   ]
-- > }
jsonFormat :: FormatFn
jsonFormat rev state = unpack . encode $ HgRevState (rev, state)


newtype HgRevState = HgRevState (HgRev, HgState)


instance ToJSON HgRevState where
    toJSON (HgRevState (r, s)) =
        object
        [ "long"      .= hgRevision r
        , "short"     .= hgShortRev r
        , "branch"    .= hgBranch r
        , "tags"      .= hgTags r
        , "bookmarks" .= hgBookmarks r
        , "clean"     .= hgIsClean s
        ]
