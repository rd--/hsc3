import Control.Monad {- base -}
import Data.List {- base -}
import Data.List.Ordered {- data-ordlist -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}
import Text.Regex {- regex-compat -}

import qualified Music.Theory.Function as T {- hmt -}

-- * pkg-dep

is_import :: String -> Bool
is_import = isPrefixOf "import"

has_comment :: String -> Bool
has_comment = T.predicate_all (map isInfixOf ["{- "," -}"])

import_pkg_regex :: Regex
import_pkg_regex = mkRegex "\\{- ([-a-zA-Z0-9]*) -\\}"

import_pkg :: String -> String
import_pkg s =
    case matchRegex import_pkg_regex s of
      Just [r] -> r
      _ -> error ("import_pkg: " ++ s)

-- > fmap hs_pkg_dep $ readFile "setup.hs"
hs_pkg_dep :: String -> [String]
hs_pkg_dep s =
    let c = filter (T.predicate_and is_import has_comment) (lines s)
    in nubSort ("base" : map import_pkg c)

hs_file_pkg_dep :: FilePath -> IO [String]
hs_file_pkg_dep = fmap hs_pkg_dep . readFile

hs_file_set_pkg_dep :: [FilePath] -> IO [String]
hs_file_set_pkg_dep sq = do
  r <- mapM hs_file_pkg_dep sq
  return (nubSort (concat r))

-- * Name

pkg_core :: [String]
pkg_core = ["hosc","hsc3"]

pkg_plain :: [String]
pkg_plain =
    ["hsc3-db","hsc3-dot"
    ,"hsc3-lang"
    ,"hsc3-plot"
    ,"hsc3-sf","hsc3-sf-hsndfile"
    ,"sc3-rdu"]

pkg_ext :: [String]
pkg_ext =
    ["after-pim"
    ,"hmeap","hmeap-utils"
    ,"hosc-json","hosc-utils"
    ,"hsc3-auditor","hsc3-cairo","hsc3-data","hsc3-rec","hsc3-rw","hsc3-unsafe","hsc3-utils"
    ,"hsdif","hsharc","hspear"
    ,"midi-osc"]

pkg_all :: [String]
pkg_all = concat [pkg_core,pkg_plain,pkg_ext]

pkg_non_hsc3 :: [String]
pkg_non_hsc3 =
    ["hmt","hly","hts"
    ,"hls","hps"
    ,"html-minus","pandoc-minus","www-minus"]

put_w :: [String] -> IO ()
put_w = putStrLn . unwords

pkg_set :: String -> [String]
pkg_set nm =
    case nm of
      "core" -> pkg_core
      "plain" -> pkg_plain
      "core+plain" -> pkg_core ++ pkg_plain
      "ext" -> pkg_ext
      "all" -> pkg_all
      _ -> error "hsc3-setup: unknown pkg_set"

is_local_pkg :: String -> Bool
is_local_pkg = flip elem (pkg_all ++ pkg_non_hsc3)

hs_file_set_pkg_dep_non_local :: [FilePath] -> IO [String]
hs_file_set_pkg_dep_non_local nm = fmap (filter (not . is_local_pkg)) (hs_file_set_pkg_dep nm)

s_cabal_print_exec :: String -> FilePath -> IO ()
s_cabal_print_exec prefix fn = do
  pkg <- hs_file_set_pkg_dep [fn]
  putStrLn (unlines [concat ["Executable         ",prefix,takeBaseName fn]
                    ,concat [" Main-Is:          ",fn]
                    ,concat [" Build-Depends:    ",intercalate "," pkg]])

s_echo :: String -> IO ()
s_echo nm = put_w (sort (pkg_set nm))

s_run :: String -> [String] -> IO ExitCode
s_run cmd arg = do
  putStrLn (unwords (cmd : arg))
  rawSystem cmd arg

s_cwd :: String -> IO ()
s_cwd dir = put_w ["cd",dir] >> setCurrentDirectory dir

s_clone :: String -> FilePath -> String -> IO ()
s_clone nm src dst = do
  let f pkg = s_run "darcs" ["get",src </> pkg]
  s_cwd dst >> mapM_ f (pkg_set nm)

s_update :: String -> FilePath -> FilePath -> IO ()
s_update nm src dst = do
  let f pkg = s_cwd (dst </> pkg) >> s_run "darcs" ["pull",src </> pkg]
  mapM_ f (pkg_set nm)

s_at_each :: String -> Maybe FilePath -> (String -> (String,[String])) -> IO ()
s_at_each nm dir gen = do
  let f pkg = let (cmd,args) = gen pkg
                  act = s_run cmd args
              in case dir of
                   Nothing -> act
                   Just dir' -> s_cwd (dir' </> pkg) >> act
  mapM_ f (pkg_set nm)

s_at_each' :: String -> Maybe FilePath -> String -> [String] -> IO ()
s_at_each' nm dir cmd args = s_at_each nm dir (const (cmd,args))

s_with_all :: String -> FilePath -> ([FilePath] -> (String, [String])) -> IO ()
s_with_all nm dir gen =
    let pkg = pkg_set nm
        pkg' = map (dir </>) pkg
        (cmd,args) = gen pkg'
    in void (s_run cmd args)

help :: [String]
help =
    ["setup {cabal|clone|echo|local|pkg-dep|rebuild|unregister|update}"
    ,"  cabal print-exec prefix hs-file"
    ,"  clone name src dst"
    ,"  echo name"
    ,"  local name directory command arg..."
    ,"  pkg-dep {-all | -non-local} hs-file..."
    ,"  rebuild name directory"
    ,"  unregister name"
    ,"  update name src dst"
    ,""
    ,"    name = core | plain | core+plain | ext | all"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    "cabal":"print-exec":prefix:fn -> mapM_ (s_cabal_print_exec prefix) fn
    ["clone",nm,src,dst] -> s_clone nm src dst
    ["echo",nm] -> s_echo nm
    "local":nm:dir:cmd:arg -> s_at_each' nm (Just dir) cmd arg
    "pkg-dep":"-all":nm -> hs_file_set_pkg_dep nm >>= putStrLn . unwords
    "pkg-dep":"-non-local":nm -> hs_file_set_pkg_dep_non_local nm >>= putStrLn . unwords
    ["rebuild",nm,dir] -> s_with_all nm dir (\pkg -> ("cabal","install" : pkg))
    ["unregister",nm] -> s_at_each nm Nothing (\pkg -> ("ghc-pkg",["unregister","--force",pkg]))
    ["update",nm,src,dst] -> s_update nm src dst
    _ -> putStrLn (unlines help)
