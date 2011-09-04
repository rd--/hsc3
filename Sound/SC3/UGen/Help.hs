module Sound.SC3.UGen.Help where

import Control.Exception
import Control.Monad
import Data.Char
import System.IO.Error
import System.Cmd {- process -}
import System.Directory {- directory -}
import System.Environment
import System.FilePath

-- Read the environment variable @SC3_HELP@, the default value is
-- @~/share/SuperCollider/Help@.
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

-- | The name of the local SC3 Help file documenting `u'.
ugenSC3HelpFile :: String -> IO FilePath
ugenSC3HelpFile u = do
  d <- sc3HelpDirectory
  c <- sc3HelpClassFile d u
  case c of
    Just c' -> return c'
    Nothing -> return (sc3HelpOperatorEntry d u)

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
