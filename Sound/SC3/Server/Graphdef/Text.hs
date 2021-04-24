module Sound.SC3.Server.Graphdef.Text where

import Data.Char {- base -}

import qualified Numeric {- base -}

import qualified Sound.OSC.Datum as Datum {- hosc -}

import Sound.SC3.Server.Graphdef {- hsc3 -}

-- | * PRINT

-- | Print string.  Strings must not have internal whitespace or semi-colons.
print_string :: Datum.ASCII -> String
print_string a =
  let s = Datum.ascii_to_string a
  in if any isSpace s || ';' `elem` s then error "print_string" else s

-- | 'ENCODE_F' for plain text output.
enc_text :: (String -> String) -> ENCODE_F String
enc_text com_f =
  (unwords . filter (not . null),print_string,show,show,show,\n -> Numeric.showFFloat Nothing n ""
  ,com_f)

{- | 'encode_graphdef_f' of 'enc_text' with optional semi-colon delimited comments.

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

{-
import qualified Control.Monad.State as S {- mtl -}

-- * LIST INPUT

-- | Read the next value from a list.
list_read :: S.State [t] t
list_read = do
  l <- S.get
  when (null l) (error "list_read")
  S.put (tail l)
  return (head l)

-- | 'flip' 'evalState'.
with_list :: [t] -> S.State [t] u -> u
with_list = flip S.evalState
-}

