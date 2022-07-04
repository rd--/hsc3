{- | .scsyndef file encoded in plain text.
There are reader and writer functions.
-}
module Sound.Sc3.Server.Graphdef.Text where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.Functor.Identity {- base -}

import qualified Numeric {- base -}

import qualified Control.Monad.State as S {- mtl -}

import qualified Sound.Osc.Datum as Datum {- hosc -}

import Sound.Sc3.Server.Graphdef {- hsc3 -}

-- | * Printer

-- | Print string.  Strings must not have internal whitespace or semi-colons.  A quoting system could allow these if required.
print_string :: Datum.Ascii -> String
print_string a =
  let s = Datum.ascii_to_string a
  in if any isSpace s || ';' `elem` s then error "print_string" else s

-- | 'Encode_Functions' for plain text output.
enc_text :: (String -> String) -> Encode_Functions String
enc_text com_f =
  (unwords . filter (not . null),print_string,show,show,show,\n -> Numeric.showFFloat Nothing n ""
  ,com_f)

{- | 'encode_graphdef_f' of 'enc_text' with optional semi-colon delimited comments.

> import Sound.Sc3.Server.Graphdef.Binary
> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= putStrLn . print_graphdef True
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"
-}
print_graphdef :: Bool -> Graphdef -> String
print_graphdef with_com =
    let com_f = if with_com then \c -> concat ["\n; ",c,"\n"] else const ""
    in encode_graphdef_f (enc_text com_f)

-- * List Input

-- | Read the next value from a list.
list_read_f :: (t -> u) -> S.State [t] u
list_read_f f = do
  l <- S.get
  when (null l) (error "list_read_f")
  S.put (tail l)
  return (f (head l))

-- | Read function for floating point that admits inf and infinity.
read_float :: (Fractional p, Read p) => String -> p
read_float txt =
  case map toLower txt of
    "inf" -> 1 / 0
    "infinity" -> 1 / 0
    _ -> read txt

-- | Get_Functions for text representation of Graphdef.
text_get_f :: Get_Functions (S.StateT [String] Identity)
text_get_f = (list_read_f Datum.ascii,list_read_f read,list_read_f read,list_read_f read,list_read_f read_float)

-- | Read text representation of Graphdef, as written by 'print_graphdef'.
--
-- > read_graphdef "1396926310 0 1 simple 2 0.0 440.0 0 0 2 SinOsc 2 2 1 0 -1 1 -1 0 2 Out 2 2 0 0 -1 0 0 0"
read_graphdef :: String -> Graphdef
read_graphdef txt =
  let delete_comments = filter (\x -> not (null x) && (head x /= ';'))
  in S.evalState (get_graphdef text_get_f) (concatMap words (delete_comments (lines txt)))

read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file = fmap read_graphdef . readFile

