{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.HgRev.TH where

import           Data.Aeson                 (ToJSON (..), encode, object, (.=))
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Data.Char                  (toLower)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Development.HgRev          (HgRev (..), HgState (..),
                                             hgIsDirty, hgRevState, hgShortRev)
import           Language.Haskell.TH        (ExpQ, runIO, stringE)
import           System.Directory           (getCurrentDirectory)


-- | Function to format hg rev and state info for printing
type FormatFn = (HgRev -> HgState -> String)


-- | Apply a format function and return a Template Haskell spliced
-- string for compiling into code.
hgRevStateTH :: FormatFn -> ExpQ
hgRevStateTH format = do
    revState <- runIO $ hgRevState =<< getCurrentDirectory
    stringE $ maybe "UNKNOWN" (uncurry format) revState


-- |
-- >  long:  d9d3a1172a1d919b3056b435891081c0d7d00599
-- >  short: d9d3a1172a1d
-- >  dirty: true
defFormat :: FormatFn
defFormat rev state
    =  "\n  long:  " <> hgRevision rev
    <> "\n  short: " <> hgShortRev rev
    <> "\n  dirty: " <> (map toLower . show $ hgIsDirty state)


-- |
-- > {
-- >   "dirty": true,
-- >   "short rev": "d9d3a1172a1d",
-- >   "branch": "default",
-- >   "bookmarks": [],
-- >   "revision": "d9d3a1172a1d919b3056b435891081c0d7d00599",
-- >   "tags": [
-- >     "tip"
-- >   ]
-- > }
jsonFormat :: FormatFn
jsonFormat rev state = unpack $ encode (rev, state)


instance ToJSON (HgRev, HgState) where
    toJSON (r, s) =
        object
        [ "revision"  .= hgRevision r
        , "short rev" .= hgShortRev r
        , "branch"    .= hgBranch r
        , "tags"      .= hgTags r
        , "bookmarks" .= hgBookmarks r
        , "dirty"     .= hgIsDirty s
        ]
