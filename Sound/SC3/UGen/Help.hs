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
-- @~\/share\/SuperCollider\/Help@.
sc3HelpDirectory :: IO String
sc3HelpDirectory = do
  r <- tryJust (guard . isDoesNotExistError) (getEnv "SC3_HELP")
  case r of
    Right v -> return v
    _ -> do h <- getEnv "HOME"
            return (h </> "share/SuperCollider/Help")

sc3HelpClassFile :: FilePath -> String -> IO (Maybe FilePath)
sc3HelpClassFile d c = do
  let f = d </> "Classes" </> c <.> "html"
  e <- doesFileExist f
  if e then return (Just f) else return Nothing

sc3HelpOperatorEntry :: FilePath -> String -> FilePath
sc3HelpOperatorEntry d o = d </> "Overviews/Operators.html#." ++ o

sc3HelpMethod :: FilePath -> Char -> (String,String) -> FilePath
sc3HelpMethod d z (c,m) = d </> "Classes" </> c <.> "html#" ++ [z] ++ m

sc3HelpClassMethod :: FilePath -> (String,String) -> FilePath
sc3HelpClassMethod d = sc3HelpMethod d '*'

sc3HelpInstanceMethod :: FilePath -> (String,String) -> FilePath
sc3HelpInstanceMethod d = sc3HelpMethod d '-'

-- | The name of the local SC3 Help file documenting `u'.  Deletes
-- @\@@ to allow use on haddock quoted comments.
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
           Nothing -> error "ugenSC3HelpFile"

toSC3Name :: String -> String
toSC3Name nm =
    case nm of
      'l':'f':nm' -> "LF"++nm'
      'p':'v':'_':nm' -> "PV_"++nm'
      p:q -> toUpper p : q
      [] -> []

-- | Use x-www-browser to view SC3 help file for `u'.
viewSC3Help :: String -> IO ()
viewSC3Help u = do
  nm <- ugenSC3HelpFile u
  void (system ("x-www-browser file://" ++ nm))
