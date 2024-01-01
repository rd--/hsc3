-- | Functions to access to the Sc3 Rtf & Html based help systems.
module Sound.Sc3.Common.Help where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}

import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Data.List.Split as Split {- split -}

import qualified Sound.Sc3.Common.Base.System as Base.System {- hsc3 -}

-- * Rtf

-- | Directory containing Sc3 Rtf help files.
sc3_rtf_help_dir :: IO FilePath
sc3_rtf_help_dir = getEnv "SC3_RTF_HELP_DIR"

{- | Find (case-insensitively) indicated file at 'sc3_rtf_help_dir'.
Runs the command "find -name" (so Unix only).

> sc3_rtf_find_file "SinOsc.help.rtf"
> sc3_rtf_find_file "lfsaw.help.rtf"
> sc3_rtf_find_file "softClip.rtf"
-}
sc3_rtf_find_file :: FilePath -> IO (Maybe FilePath)
sc3_rtf_find_file fn = do
  d <- sc3_rtf_help_dir
  r <- System.Process.readProcess "find" [d, "-iname", fn] ""
  case lines r of
    [] -> return Nothing
    [r0] -> return (Just r0)
    _ -> error "sc3_rtf_find_file: multiple files?"

-- | 'error' variant.
sc3_rtf_find_file_err :: FilePath -> IO FilePath
sc3_rtf_find_file_err = fmap (fromMaybe (error "sc3_rtf_find_file")) . sc3_rtf_find_file

-- | Run the command unrtf (so UNIX only) to convert an RTF file to a TEXT (.scd) file.
sc3_rtf_to_scd :: FilePath -> FilePath -> IO ()
sc3_rtf_to_scd rtf_fn scd_fn = do
  txt <- System.Process.readProcess "unrtf" ["--text", rtf_fn] ""
  let delete_trailing_whitespace = reverse . dropWhile isSpace . reverse
      tidy = unlines . map delete_trailing_whitespace . drop 4 . lines
  writeFile scd_fn (tidy txt)

-- | 'sc3_rtf_to_scd' of 'sc3_rtf_find_file_err', writing output to TMPDIR
sc3_rtf_help_translate :: String -> IO FilePath
sc3_rtf_help_translate nm = do
  tmp <- Base.System.get_env_default "TMPDIR" "/tmp"
  rtf_fn <- sc3_rtf_find_file_err (nm <.> "*rtf")
  let scd_fn = tmp </> takeFileName rtf_fn -<.> "scd"
  sc3_rtf_to_scd rtf_fn scd_fn
  return scd_fn

-- | 'sc3_rtf_help_translate' and run editor.
sc3_rtf_help_scd_open :: (String, [String]) -> String -> IO ()
sc3_rtf_help_scd_open (cmd, arg) nm = do
  scd_fn <- sc3_rtf_help_translate nm
  System.Process.callProcess cmd (arg ++ [scd_fn])

{- | 'sc3_rtf_help_scd_open' with emacsclient --no-wait.

> sc3_rtf_help_scd_open_emacs "lfsaw"
-}
sc3_rtf_help_scd_open_emacs :: String -> IO ()
sc3_rtf_help_scd_open_emacs = sc3_rtf_help_scd_open ("emacsclient", ["--no-wait"])

-- * Sc-Doc (Html)

-- | Url for online Sc-Doc SuperCollider documentation.
sc3_scdoc_help_url :: String
sc3_scdoc_help_url = "http://doc.sccode.org/"

{- | Read the environment variable @SC3_SCDOC_HTML_HELP_DIR@.
  The default value is @~\/.local\/share\/SuperCollider/Help@.
-}
sc3_scdoc_help_dir :: IO String
sc3_scdoc_help_dir = do
  h <- getEnv "HOME"
  let d = h </> ".local/share/SuperCollider/Help"
  Base.System.get_env_default "SC3_SCDOC_HTML_HELP_DIR" d

{- | Path to indicated Sc3 class help file.

>>> sc3_scdoc_help_class "SinOsc"
"Classes/SinOsc.html"
-}
sc3_scdoc_help_class :: String -> String
sc3_scdoc_help_class c = "Classes" </> c <.> "html"

{- | Generate path to indicated Sc3 operator help file.

>>> sc3_scdoc_help_operator "+"
"Overviews/Operators.html#+"
-}
sc3_scdoc_help_operator :: String -> FilePath
sc3_scdoc_help_operator = (++) "Overviews/Operators.html#"

{- | Generate path to indicated Sc3 method help.

>>> sc3_scdoc_help_method '*' ("C","m")
"Classes/C.html#*m"
-}
sc3_scdoc_help_method :: Char -> (String, String) -> FilePath
sc3_scdoc_help_method z (c, m) = "Classes" </> c <.> "html#" ++ [z] ++ m

{- | Generate path to indicated Sc3 class method help.

>>> sc3_scdoc_help_class_method ("C","m")
"Classes/C.html#*m"
-}
sc3_scdoc_help_class_method :: (String, String) -> FilePath
sc3_scdoc_help_class_method = sc3_scdoc_help_method '*'

{- | Generate path to indicated Sc3 instance method help.

>>> sc3_scdoc_help_instance_method ("C","m")
"Classes/C.html#-m"
-}
sc3_scdoc_help_instance_method :: (String, String) -> FilePath
sc3_scdoc_help_instance_method = sc3_scdoc_help_method '-'

{- | Sc3 help path documenting x.

>>> sc3_scdoc_help_path "Operator.distort"
"Overviews/Operators.html#distort"

>>> sc3_scdoc_help_path "Collection.*fill"
"Classes/Collection.html#*fill"

>>> sc3_scdoc_help_path "Collection.inject"
"Classes/Collection.html#-inject"

>>> sc3_scdoc_help_path "SinOsc"
"Classes/SinOsc.html"
-}
sc3_scdoc_help_path :: String -> String
sc3_scdoc_help_path s = do
  case Split.splitOn "." s of
    ["Operator", m] -> sc3_scdoc_help_operator m
    [c, '*' : m] -> sc3_scdoc_help_class_method (c, m)
    [c, m] -> sc3_scdoc_help_instance_method (c, m)
    _ -> sc3_scdoc_help_class s

{- | Open SC3 help path, either the local file or the online version.
     Use @BROWSER@ or @x-www-browser@.

> Base.System.get_env_default "BROWSER" "x-www-browser"

> sc3_scdoc_help_open True (sc3_scdoc_help_path "SinOsc")
> sc3_scdoc_help_open True (sc3_scdoc_help_path "Collection.*fill")
> sc3_scdoc_help_open False (sc3_scdoc_help_path "Collection.inject")
-}
sc3_scdoc_help_open :: Bool -> String -> IO ()
sc3_scdoc_help_open use_loc p = do
  d <- sc3_scdoc_help_dir
  b <- Base.System.get_env_default "BROWSER" "x-www-browser"
  let u = if use_loc then "file://" ++ (d </> p) else sc3_scdoc_help_url ++ p
  void (System.Process.rawSystem b [u])

{- | Generate path to indicated Sc3 instance method help.
Adds initial forward slash if not present.

> let r = "Reference/Server-Command-Reference.html#/b_alloc"
> sc3_scdoc_help_server_command_path "b_alloc" == r
-}
sc3_scdoc_help_server_command_path :: String -> FilePath
sc3_scdoc_help_server_command_path c =
  let c' = case c of
        '/' : _ -> c
        _ -> '/' : c
  in "Reference/Server-Command-Reference.html" ++ ('#' : c')

{- | 'sc3_scdoc_help_open' of 'sc3_server_command_path'

> sc3_scdoc_help_server_command_open True "s_new"
> sc3_scdoc_help_server_command_open False "/b_allocRead"
-}
sc3_scdoc_help_server_command_open :: Bool -> String -> IO ()
sc3_scdoc_help_server_command_open use_loc =
  sc3_scdoc_help_open use_loc
    . sc3_scdoc_help_server_command_path

-- * Fragments

-- | Apply function at lines of string.
on_lines_of :: ([String] -> [[String]]) -> String -> [String]
on_lines_of f = map unlines . f . lines

{- | Split text into fragments at empty lines.
Hsc3 (and related projects) write help files as sets of distinct fragments.
Fragments are separated by empty lines.
A line containing the special character sequence ---- indicates the end of the fragments.

>>> on_lines_of split_multiple_fragments ";a\nb\n\n\n;c\nd"
[";a\nb\n",";c\nd\n"]
-}
split_multiple_fragments :: [String] -> [[String]]
split_multiple_fragments = filter (not . null) . Split.splitOn [[]]

-- | The text ---- indicates the end of graph fragments.
drop_post_graph_section :: [String] -> [String]
drop_post_graph_section = takeWhile (not . isInfixOf "----")

{- | Some help files are in Markdown format.
These are recognised by examing the first character, which must be a '#'.
-}
is_md_help :: String -> Bool
is_md_help x =
  case x of
    '#' : _ -> True
    _ -> False

{- | There are two code block formats in markdown help files.
The first indents the block using a single tab or four spaces.
The second marks the start and end of the block by lines starting with three back ticks (`).

See:
<https://spec.commonmark.org/0.30/#indented-code-blocks>
and
<https://spec.commonmark.org/0.30/#fenced-code-blocks>
-}
data CodeBlockType = IndentedCodeBlock | FencedCodeBlock
  deriving (Bounded, Enum, Eq, Read, Show)

{- | Get code blocks from Markdown help file. -}
md_help_get_code_blocks :: [String] -> [(CodeBlockType, [String])]
md_help_get_code_blocks x =
  case x of
    [] -> []
    "```" : x' ->
      let (q, x'') = break (== "```") x'
      in (FencedCodeBlock,q) : md_help_get_code_blocks (drop 1 x'')
    ('\t' : _) : _ ->
      let (q, x') = span ((==) ['\t'] . take 1) x
      in (IndentedCodeBlock,(map (drop 1) q)) : md_help_get_code_blocks x'
    _ : x' ->
      md_help_get_code_blocks x'

{- | Get indented code blocks from Markdown help file.

>>> s <- readFile "/home/rohan/sw/spl/help/SuperCollider/Reference/AllpassC.help.sl"
>>> is_md_help s
True

>>> let b = md_help_get_tab_indented_code_blocks (lines s)
>>> length b
3
-}
md_help_get_tab_indented_code_blocks :: [String] -> [[String]]
md_help_get_tab_indented_code_blocks = map snd . filter ((== IndentedCodeBlock) . fst) . md_help_get_code_blocks

get_help_file_fragments :: String -> [String]
get_help_file_fragments s =
  if is_md_help s
  then on_lines_of md_help_get_tab_indented_code_blocks s
  else on_lines_of (split_multiple_fragments . drop_post_graph_section) s

-- | Read text fragments from file.
read_file_fragments :: FilePath -> IO [String]
read_file_fragments = fmap get_help_file_fragments . readFile

-- | Read text fragments from set of files.
read_file_set_fragments :: [FilePath] -> IO [String]
read_file_set_fragments = fmap concat . mapM read_file_fragments
