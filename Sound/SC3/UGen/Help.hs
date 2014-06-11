-- | Functions to provide mediated access to the SC3 help system.
module Sound.SC3.UGen.Help where

import Control.Exception
import Control.Monad
import Data.List.Split {- split -}
import Data.Maybe
import System.IO.Error
import System.Cmd {- process -}
import System.Directory {- directory -}
import System.Environment
import System.FilePath {- filepath -}

-- | Guarded variant of 'getEnv' with default value.
get_env_default :: String -> String -> IO String
get_env_default e k = do
  r <- tryJust (guard . isDoesNotExistError) (getEnv e)
  case r of
    Right v -> return v
    _ -> return k

-- | 'lookupEnv' with default value.
--
-- > lookup_env_default "PATH" "/usr/bin"
lookup_env_default :: String -> String -> IO String
lookup_env_default e k = fmap (fromMaybe k) (lookupEnv e)

-- | Read the environment variable @SC3_HELP@, the default value is
-- @~\/.local\/share\/SuperCollider/Help@.
sc3HelpDirectory :: IO String
sc3HelpDirectory = do
  h <- getEnv "HOME"
  let d = h </> ".local/share/SuperCollider/Help"
  get_env_default "SC3_HELP" d

-- | Locate path to indicated SC3 class help file.
--
-- > import System.FilePath
-- >
-- > d <- sc3HelpDirectory
-- > h <- sc3HelpClassFile d "SinOsc"
-- > h == Just (d </> "Classes/SinOsc.html")
sc3HelpClassFile :: FilePath -> String -> IO (Maybe FilePath)
sc3HelpClassFile d c = do
  let f = d </> "Classes" </> c <.> "html"
  e <- doesFileExist f
  return (if e then Just f else Nothing)

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
-- > import Sound.SC3.UGen.Name
-- >
-- > ugenSC3HelpFile "Collection.*fill"
-- > ugenSC3HelpFile "Collection.inject"
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

-- | Use @BROWSER@ or @x-www-browser@ to view SC3 help file for `u'.
--
-- > get_env_default "BROWSER" "x-www-browser"
--
-- > import Sound.SC3.UGen.Name
-- >
-- > viewSC3Help (toSC3Name "Collection.*fill")
-- > viewSC3Help (toSC3Name "Collection.inject")
-- > viewSC3Help (toSC3Name "sinOsc")
viewSC3Help :: String -> IO ()
viewSC3Help u = do
  nm <- ugenSC3HelpFile u
  br <- get_env_default "BROWSER" "x-www-browser"
  void (rawSystem br ["file://" ++ nm])
