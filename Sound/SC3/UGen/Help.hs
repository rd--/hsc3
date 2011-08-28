module Sound.SC3.UGen.Help where

import Control.Exception
import Control.Monad
import Data.Char
import System.IO.Error
import System.Cmd {- process -}
import System.Environment

-- Read the environment variable @SC3_HELP@, the default value is
-- @~/share/SuperCollider/Help@.
sc3HelpDirectory :: IO String
sc3HelpDirectory = do
  h <- getEnv "HOME"
  r <- tryJust (guard . isDoesNotExistError) (getEnv "SC3_HELP")
  case r of
    Right v -> return v
    _ -> return (h ++ "/share/SuperCollider/Help")

-- | The name of the local SC3 Help file documenting `u'.
ugenSC3HelpFile :: String -> IO FilePath
ugenSC3HelpFile u = do
    d <- sc3HelpDirectory
    return (d ++ "/Classes/" ++ u ++ ".html")

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
  _ <- system ("x-www-browser " ++ nm)
  return ()
