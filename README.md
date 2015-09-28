# HgRev

## Compile Mercurial (hg) version info into Haskell code

- [HgRev Website](https://github.com/LukeHoersten/hgrev)
- [HgRev Hackage](https://hackage.haskell.org/package/hgrev)
- [Mercurial (hg)](https://mercurial.selenic.com)

### Overview

`hgrev` provides two modules:
- `Development.HgRev` -
  [Mercurial (hg)](https://mercurial.selenic.com) Haskell API
- `Development.HgRev.TH` - Template Haskell splice to compile version
  info into Haskell code

Use `$(hgRevStateTH defFormat)` with Template Haskell enabled to
insert the formatted version string.

### Requirements

`hgrev` requires the `hg` binary is installed and available on the
system.  `Development.HgRev.HgRev` and `Development.HgRev.HgState` are
obtained via two separate calls to `hg` because working directory
state isn't available programmatically.

### Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Example where

import           Data.Monoid          ((<>))
import           Data.Text            (Text, pack)
import           Development.HgRev.TH (defFormat, hgRevStateTH, jsonFormat)
import           Options.Applicative  (Parser, ParserInfo, execParser, fullDesc,
                                        help, helper, info, infoOption, long,
                                        progDesc, short)

main :: IO ()
main = execParser parserInfo >> return ()

verSwitch :: Parser (a -> a)
verSwitch =
    infoOption ("HG rev: " <> $(hgRevStateTH defFormat))
    $  long "version"
    <> short 'v'
    <> help "Display version information"

jsonSwitch :: Parser (a -> a)
jsonSwitch =
    infoOption $(hgRevStateTH jsonFormat)
    $  long "json"
    <> short 'J'
    <> help "Display JSON version information"

parserInfo :: ParserInfo (a -> a)
parserInfo = info (helper <*> verSwitch <* jsonSwitch) fullDesc
```

Check out the [gitrev](https://hackage.haskell.org/package/gitrev)
package for similar git functionality.
