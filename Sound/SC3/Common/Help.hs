-- | Functions to access to the SC3 RTF & HTML based help systems.
module Sound.SC3.Common.Help where

import Control.Monad {- base -}
import Data.Char {- base -}
import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Data.List.Split as Split {- split -}

import qualified Sound.SC3.Common.Base as Base {- hsc3 -}

-- * RTF

-- | Directory containing SC3 RTF help files.
sc3_rtf_help_dir :: IO FilePath
sc3_rtf_help_dir = getEnv "SC3_RTF_HELP_DIR"

{- | Find (case-insensitively) indicated file at 'sc3_rtf_help_dir'.
     Runs the command "find -name" (so UNIX only).

> sc3_rtf_find_file "SinOsc.help.rtf"
> sc3_rtf_find_file "lfsaw.help.rtf"
> sc3_rtf_find_file "softClip.rtf"
-}
sc3_rtf_find_file :: FilePath -> IO (Maybe FilePath)
sc3_rtf_find_file fn = do
  d <- sc3_rtf_help_dir
  r <- System.Process.readProcess "find" [d,"-iname",fn] ""
  case lines r of
    [] -> return Nothing
    [r0] -> return (Just r0)
    _ -> error "sc3_rtf_find_file: multiple files?"

-- | 'error' variant.
sc3_rtf_find_file_err :: FilePath -> IO FilePath
sc3_rtf_find_file_err = fmap (maybe (error "sc3_rtf_find_file") id) . sc3_rtf_find_file

-- | Run the command unrtf (so UNIX only) to convert an RTF file to a TEXT (.scd) file.
sc3_rtf_to_scd :: FilePath -> FilePath -> IO ()
sc3_rtf_to_scd rtf_fn scd_fn = do
  txt <- System.Process.readProcess "unrtf" ["--text",rtf_fn] ""
  let delete_trailing_whitespace = reverse . dropWhile isSpace . reverse
      tidy = unlines . map delete_trailing_whitespace . drop 4 . lines
  writeFile scd_fn (tidy txt)

-- | 'sc3_rtf_to_scd' of 'sc3_rtf_find_file_err', writing output to TMPDIR
sc3_rtf_help_translate :: String -> IO FilePath
sc3_rtf_help_translate nm = do
  tmp <- Base.get_env_default "TMPDIR" "/tmp"
  rtf_fn <- sc3_rtf_find_file_err (nm <.> "*rtf")
  let scd_fn = tmp </> takeFileName rtf_fn -<.> "scd"
  sc3_rtf_to_scd rtf_fn scd_fn
  return scd_fn

-- | 'sc3_rtf_help_translate' and run editor.
sc3_rtf_help_scd_open :: (String,[String]) -> String -> IO ()
sc3_rtf_help_scd_open (cmd,arg) nm = do
  scd_fn <- sc3_rtf_help_translate nm
  System.Process.callProcess cmd (arg ++ [scd_fn])

-- | 'sc3_rtf_help_scd_open' with emacsclient --no-wait.
--
-- > sc3_rtf_help_scd_open_emacs "lfsaw"
sc3_rtf_help_scd_open_emacs :: String -> IO ()
sc3_rtf_help_scd_open_emacs = sc3_rtf_help_scd_open ("emacsclient",["--no-wait"])

-- * SC-DOC

-- | URL for online SC-DOC SuperCollider documentation.
sc3_scdoc_help_url :: String
sc3_scdoc_help_url = "http://doc.sccode.org/"

-- | Read the environment variable @SC3_SCDOC_HTML_HELP_DIR@.
--   The default value is @~\/.local\/share\/SuperCollider/Help@.
sc3_scdoc_help_dir :: IO String
sc3_scdoc_help_dir = do
  h <- getEnv "HOME"
  let d = h </> ".local/share/SuperCollider/Help"
  Base.get_env_default "SC3_SCDOC_HTML_HELP_DIR" d

-- | Path to indicated SC3 class help file.
--
-- > sc3_scdoc_help_class "SinOsc" == "Classes/SinOsc.html"
sc3_scdoc_help_class :: String -> String
sc3_scdoc_help_class c = "Classes" </> c <.> "html"

-- | Generate path to indicated SC3 operator help file.
--
-- > sc3_scdoc_help_operator "+" == "Overviews/Operators.html#.+"
sc3_scdoc_help_operator :: String -> FilePath
sc3_scdoc_help_operator o = "Overviews/Operators.html#." ++ o

-- | Generate path to indicated SC3 method help.
--
-- > sc3_scdoc_help_method '*' ("C","m") == "Classes/C.html#*m"
sc3_scdoc_help_method :: Char -> (String,String) -> FilePath
sc3_scdoc_help_method z (c,m) = "Classes" </> c <.> "html#" ++ [z] ++ m

-- | Generate path to indicated SC3 class method help.
--
-- > sc3_scdoc_help_class_method ("C","m") == "Classes/C.html#*m"
sc3_scdoc_help_class_method :: (String,String) -> FilePath
sc3_scdoc_help_class_method = sc3_scdoc_help_method '*'

-- | Generate path to indicated SC3 instance method help.
--
-- > sc3_scdoc_help_instance_method ("C","m") == "Classes/C.html#-m"
sc3_scdoc_help_instance_method :: (String,String) -> FilePath
sc3_scdoc_help_instance_method = sc3_scdoc_help_method '-'

{- | SC3 help path documenting x.

> sc3_scdoc_help_path "Collection.*fill" == "Classes/Collection.html#*fill"
> sc3_scdoc_help_path "Collection.inject" == "Classes/Collection.html#-inject"
> sc3_scdoc_help_path "SinOsc" == "Classes/SinOsc.html"
-}
sc3_scdoc_help_path :: String -> String
sc3_scdoc_help_path s = do
  case Split.splitOn "." s of
    ["Operator",m] -> sc3_scdoc_help_operator m
    [c,'*':m] -> sc3_scdoc_help_class_method (c,m)
    [c,m] -> sc3_scdoc_help_instance_method (c,m)
    _ -> sc3_scdoc_help_class s

{- | Open SC3 help path, either the local file or the online version.
     Use @BROWSER@ or @x-www-browser@.

> Base.get_env_default "BROWSER" "x-www-browser"

> sc3_scdoc_help_open True (sc3_scdoc_help_path "SinOsc")
> sc3_scdoc_help_open True (sc3_scdoc_help_path "Collection.*fill")
> sc3_scdoc_help_open False (sc3_scdoc_help_path "Collection.inject")
-}
sc3_scdoc_help_open :: Bool -> String -> IO ()
sc3_scdoc_help_open use_loc p = do
  d <- sc3_scdoc_help_dir
  b <- Base.get_env_default "BROWSER" "x-www-browser"
  let u = if use_loc then "file://" ++ (d </> p) else sc3_scdoc_help_url ++ p
  void (System.Process.rawSystem b [u])

{- | Generate path to indicated SC3 instance method help.
     Adds initial forward slash if not present.

> let r = "Reference/Server-Command-Reference.html#/b_alloc"
> sc3_scdoc_help_server_command_path "b_alloc" == r

-}
sc3_scdoc_help_server_command_path :: String -> FilePath
sc3_scdoc_help_server_command_path c =
    let c' = case c of
               '/':_ -> c
               _ -> '/':c
    in "Reference/Server-Command-Reference.html" ++ ('#' : c')

{- | 'sc3_scdoc_help_open' of 'sc3_server_command_path'

> sc3_scdoc_help_server_command_open True "s_new"
> sc3_scdoc_help_server_command_open False "/b_allocRead"
-}
sc3_scdoc_help_server_command_open :: Bool -> String -> IO ()
sc3_scdoc_help_server_command_open use_loc =
  sc3_scdoc_help_open use_loc .
  sc3_scdoc_help_server_command_path
