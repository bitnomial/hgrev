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


hgRevStateTH :: (HgRev -> HgState -> String) -> ExpQ
hgRevStateTH format = do
    revState <- runIO $ hgRevState =<< getCurrentDirectory
    stringE $ maybe "UNKNOWN" (uncurry format) revState


defFormat :: HgRev -> HgState -> String
defFormat rev state
    =  "\n  long:  " <> hgRevision rev
    <> "\n  short: " <> hgShortRev rev
    <> "\n  dirty: " <> (map toLower . show $ hgIsDirty state)


jsonFormat :: HgRev -> HgState -> String
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
