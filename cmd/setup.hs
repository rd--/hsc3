import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.IO.Unsafe {- base -}
import System.Process {- process -}

import qualified Data.List.Split as Split {- split -}
import qualified Data.List.Ordered as Ordered {- data-ordlist -}
import qualified Text.Regex as Regex {- regex-compat -}

-- * UTIL

predicate_all :: [t -> Bool] -> t -> Bool
predicate_all p x = all id (map ($ x) p)

predicate_and :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predicate_and f g x = f x && g x

-- * pkg-dep

is_import :: String -> Bool
is_import = isPrefixOf "import"

has_comment :: String -> Bool
has_comment = predicate_all (map isInfixOf ["{- "," -}"])

import_pkg_regex :: Regex.Regex
import_pkg_regex = Regex.mkRegex "\\{- ([-a-zA-Z0-9]*) -\\}"

import_pkg :: String -> String
import_pkg s =
    case Regex.matchRegex import_pkg_regex s of
      Just [r] -> r
      _ -> error ("import_pkg: " ++ s)

-- > fmap hs_pkg_dep $ readFile "setup.hs"
hs_pkg_dep :: String -> [String]
hs_pkg_dep s =
    let c = filter (predicate_and is_import has_comment) (lines s)
    in Ordered.nubSort ("base" : map import_pkg c)

hs_file_pkg_dep :: FilePath -> IO [String]
hs_file_pkg_dep = fmap hs_pkg_dep . readFile

hs_file_set_pkg_dep :: [FilePath] -> IO [String]
hs_file_set_pkg_dep sq = do
  r <- mapM hs_file_pkg_dep sq
  return (Ordered.nubSort (concat r))

-- * Name

prj_dir :: FilePath
prj_dir = "/home/rohan/sw/hsc3/"

prj_file :: FilePath -> FilePath
prj_file = (++) prj_dir

setup_db :: FilePath
setup_db = prj_file "db/setup.db"

split_once :: Eq a => a -> [a] -> ([a], [a])
split_once e l = let (p,_:q) = break ((==) e) l in (p,q)

type PKG_TBL = [(String,[String])]

-- > s <- readFile setup_db
-- > db_parse s
db_parse :: String -> PKG_TBL
db_parse s =
    let f l = let (nm,sq) = split_once '=' l
              in (nm,Split.splitOn "," sq)
    in map f (lines s)

pkg_tbl_io :: IO PKG_TBL
pkg_tbl_io = fmap db_parse (readFile setup_db)

pkg_tbl :: PKG_TBL
pkg_tbl = unsafePerformIO pkg_tbl_io

lookup_err :: (Eq k,Show k) => k -> [(k,v)] -> v
lookup_err k = fromMaybe (error (show ("lookup_err",k))) . lookup k

pkg_set :: String -> [String]
pkg_set nm =
  case nm of
    "all" -> concat (map snd (filter ((/= "remote") . fst) pkg_tbl))
    _ -> lookup_err nm pkg_tbl

pkg_grp :: String -> String
pkg_grp nm =
  case find (\(_,set) -> nm `elem` set) pkg_tbl of
    Nothing -> "non-local"
    Just (grp,_) -> grp

put_w :: [String] -> IO ()
put_w = putStrLn . unwords

is_local_pkg :: String -> Bool
is_local_pkg = (`notElem` ["non-local","remote"]) . pkg_grp

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
    ,"    name = all | " ++ intercalate " | " (map fst pkg_tbl)]

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
    ["rebuild",nm,dir] -> s_with_all nm dir (\pkg -> ("cabal","v1-install" : pkg))
    ["unregister",nm] -> s_at_each nm Nothing (\pkg -> ("ghc-pkg",["unregister","--force",pkg]))
    ["update",nm,src,dst] -> s_update nm src dst
    _ -> putStrLn (unlines help)
