-- | Encoding agnostic i/o.
module Sound.SC3.Server.Graphdef.IO where

import Data.List {- base -}

import Sound.SC3.Server.Graphdef {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Binary as Binary {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Text as Text {- hsc3 -}

{- | Encoding agnostic file reader.
If the file has a .scsyndef.text extension it's read as a text file, else as a binary file.

read_graphdef_file "/tmp/stsc3.scsyndef.text"
-}
read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file fn =
  if ".scsyndef.text" `isSuffixOf` fn
  then Text.read_graphdef_file fn
  else Binary.read_graphdef_file fn
