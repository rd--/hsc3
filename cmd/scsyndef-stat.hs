import System.Environment {- base -}

import qualified Sound.SC3.Server.Synthdef as S {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef as G {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Read as R {- hsc3 -}

-- > let sy = "/home/rohan/sw/hsc3-graphs/scsyndef/why_supercollider.scsyndef"
-- > scsyndef_stat sy "/dev/stdout"
scsyndef_stat :: FilePath -> FilePath -> IO ()
scsyndef_stat sy_nm st_nm = do
  d <- G.read_graphdef_file sy_nm
  let (_,g) = R.graphdef_to_graph d
  writeFile st_nm (unlines (S.graph_stat g))

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> scsyndef_stat "/dev/stdin" "/dev/stdout"
    [sy] -> scsyndef_stat sy "/dev/stdout"
    [sy,st] -> scsyndef_stat sy st
    _ -> putStrLn "scsyndef-stat scyndef [stat]"
