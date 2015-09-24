module Development.HgRev.TH where

import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Development.HgRev   (HgRev (..), HgState (..), hgRevState)
import           Language.Haskell.TH (ExpQ, runIO, stringE)
import           System.Directory    (getCurrentDirectory)


hgRevStateTH :: (HgRev -> HgState -> String) -> ExpQ
hgRevStateTH format = do
    revState <- runIO $ hgRevState =<< getCurrentDirectory
    stringE $ "hg rev: " <> maybe "UNKNOWN" (uncurry format) revState


defFormat :: HgRev -> HgState -> String
defFormat rev state =
    "\n\t long: " <> hgRevision rev
    <> "\n\t short: " <> take 12 (hgRevision rev)
    <> "\n\t dirty: " <> dirty state
  where
    dirty Clean = "false"
    dirty Dirty = "true"
