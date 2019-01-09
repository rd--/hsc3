import System.Environment {- base -}

import qualified Sound.SC3.Server.Graphdef as Graphdef {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Read as Read {- hsc3 -}

-- > let sy = "/home/rohan/sw/hsc3-graphs/scsyndef/why-supercollider-rand.sc.scsyndef"
-- > scsyndef_stat sy "/dev/stdout"
scsyndef_stat :: FilePath -> FilePath -> IO ()
scsyndef_stat sy_nm st_nm = do
  str <- Graphdef.scsyndef_stat sy_nm
  writeFile st_nm str

-- > scsyndef_ug_stat sy "/dev/stdout"
scsyndef_ug_stat :: FilePath -> FilePath -> IO ()
scsyndef_ug_stat sy_nm st_nm = do
  str <- Read.scsyndef_ug_stat sy_nm
  writeFile st_nm str

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> scsyndef_ug_stat "/dev/stdin" "/dev/stdout"
    [sy] -> scsyndef_ug_stat sy "/dev/stdout"
    [sy,st] -> scsyndef_ug_stat sy st
    _ -> putStrLn "scsyndef-stat scyndef [stat]"
