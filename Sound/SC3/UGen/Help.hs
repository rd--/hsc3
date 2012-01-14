-- | Functions to provide mediated access to the SC3 help system.
module Sound.SC3.UGen.Help where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List.Split {- split -}
import System.IO.Error
import System.Cmd {- process -}
import System.Directory {- directory -}
import System.Environment
import System.FilePath

-- | Read the environment variable @SC3_HELP@, the default value is
-- @~\/.local\/share\/SuperCollider/Help@.
sc3HelpDirectory :: IO String
sc3HelpDirectory = do
  r <- tryJust (guard . isDoesNotExistError) (getEnv "SC3_HELP")
  case r of
    Right v -> return v
    _ -> do h <- getEnv "HOME"
            return (h </> ".local/share/SuperCollider/Help")

-- | Locate path to indicated SC3 class help file.
--
-- > sc3HelpDirectory >>= (flip sc3HelpClassFile) "SinOsc"
sc3HelpClassFile :: FilePath -> String -> IO (Maybe FilePath)
sc3HelpClassFile d c = do
  let f = d </> "Classes" </> c <.> "html"
  e <- doesFileExist f
  if e then return (Just f) else return Nothing

-- | Generate path to indicated SC3 operator help file.
--
-- > sc3HelpOperatorEntry "." "+" == "./Overviews/Operators.html#.+"
sc3HelpOperatorEntry :: FilePath -> String -> FilePath
sc3HelpOperatorEntry d o = d </> "Overviews/Operators.html#." ++ o

-- | Generate path to indicated SC3 method help.
--
-- > sc3HelpMethod "." '*' ("C","m") == "./Classes/C.html#*m"
sc3HelpMethod :: FilePath -> Char -> (String,String) -> FilePath
sc3HelpMethod d z (c,m) = d </> "Classes" </> c <.> "html#" ++ [z] ++ m

-- | Generate path to indicated SC3 class method help.
--
-- > sc3HelpClassMethod "." ("C","m") == "./Classes/C.html#*m"
sc3HelpClassMethod :: FilePath -> (String,String) -> FilePath
sc3HelpClassMethod d = sc3HelpMethod d '*'

-- | Generate path to indicated SC3 instance method help.
--
-- > sc3HelpInstanceMethod "." ("C","m") == "./Classes/C.html#-m"
sc3HelpInstanceMethod :: FilePath -> (String,String) -> FilePath
sc3HelpInstanceMethod d = sc3HelpMethod d '-'

-- | The name of the local SC3 Help file documenting `u'.  Deletes
-- @\@@ to allow use on haddock quoted comments.
--
-- > ugenSC3HelpFile (toSC3Name "Collection.*fill")
-- > ugenSC3HelpFile (toSC3Name "Collection.inject")
-- > ugenSC3HelpFile (toSC3Name "sinOsc")
ugenSC3HelpFile :: String -> IO FilePath
ugenSC3HelpFile x = do
  let s = filter (`notElem` "@") x
  d <- sc3HelpDirectory
  cf <- sc3HelpClassFile d s
  case splitOn "." s of
    ["Operator",m] -> return (sc3HelpOperatorEntry d m)
    [c,'*':m] -> return (sc3HelpClassMethod d (c,m))
    [c,m] -> return (sc3HelpInstanceMethod d (c,m))
    _ -> case cf of
           Just cf' -> return cf'
           Nothing -> error (show ("ugenSC3HelpFile",d,cf,x,s))

-- | Convert from hsc3 name to SC3 name.
--
-- > toSC3Name "sinOsc" == "SinOsc"
-- > toSC3Name "lfSaw" == "LFSaw"
-- > toSC3Name "pv_Copy" == "PV_Copy"
toSC3Name :: String -> String
toSC3Name nm =
    case nm of
      'l':'f':nm' -> "LF"++nm'
      'p':'v':'_':nm' -> "PV_"++nm'
      p:q -> toUpper p : q
      [] -> []

-- | Use x-www-browser to view SC3 help file for `u'.
--
-- > viewSC3Help (toSC3Name "Collection.*fill")
-- > viewSC3Help (toSC3Name "Collection.inject")
-- > viewSC3Help (toSC3Name "sinOsc")
viewSC3Help :: String -> IO ()
viewSC3Help u = do
  nm <- ugenSC3HelpFile u
  void (system ("x-www-browser file://" ++ nm))
