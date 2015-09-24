module Development.HgRev.TH where

import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Development.HgRev   (HgRev (..), HgState (..), hgRevState)
import           Language.Haskell.TH (ExpQ, Q, runIO, stringE)
import           System.Directory    (getCurrentDirectory)


hgRevStateTH :: ExpQ
hgRevStateTH = do
    cwd <- runIO getCurrentDirectory
    revState <- runIO $ hgRevState cwd
    stringE $ "hg rev: " <> maybe "UNKNOWN" (uncurry formatRevState) revState


formatRevState :: HgRev -> HgState -> String
formatRevState rev state
    = "long: " <> hgRevision rev
      <> " short: " <> take 12 (hgRevision rev)
      <> " dirty: " <> dirty state
  where dirty Clean = "false"
        dirty Dirty = "true"
