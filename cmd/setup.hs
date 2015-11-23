import Control.Monad {- base -}
import Data.List {- base -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

core :: [String]
core = ["hosc","hsc3"]

plain :: [String]
plain =
    ["hsc3-db","hsc3-dot"
    ,"hsc3-lang"
    ,"hsc3-plot"
    ,"hsc3-sf","hsc3-sf-hsndfile"
    ,"sc3-rdu"]

ext :: [String]
ext =
    ["after-pim"
    ,"hmeap","hmeap-utils"
    ,"hosc-json","hosc-utils"
    ,"hsc3-auditor","hsc3-cairo","hsc3-data","hsc3-rec","hsc3-rw","hsc3-unsafe","hsc3-utils"
    ,"hsdif","hsharc","hspear"]

put_w :: [String] -> IO ()
put_w = putStrLn . unwords

pkg_set :: String -> [String]
pkg_set nm =
    case nm of
      "core" -> core
      "plain" -> plain
      "core+plain" -> core ++ plain
      "ext" -> ext
      "all" -> concat [core,plain,ext]
      _ -> error "hsc3-setup: unknown pkg_set"

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
    ["setup {clone|echo|local|rebuild|unregister|update}"
    ,"  clone name src dst"
    ,"  echo name"
    ,"  local name directory command arg..."
    ,"  rebuild name directory"
    ,"  unregister name"
    ,"  update name src dst"
    ,""
    ,"    name = core | plain | core+plain | ext | all"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["clone",nm,src,dst] -> s_clone nm src dst
    ["echo",nm] -> s_echo nm
    "local":nm:dir:cmd:arg -> s_at_each' nm (Just dir) cmd arg
    ["rebuild",nm,dir] -> s_with_all nm dir (\pkg -> ("cabal","install" : pkg))
    ["unregister",nm] -> s_at_each nm Nothing (\pkg -> ("ghc-pkg",["unregister","--force",pkg]))
    ["update",nm,src,dst] -> s_update nm src dst
    _ -> putStrLn (unlines help)
