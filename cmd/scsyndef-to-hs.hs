import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Sound.SC3.UGen.Graph.Reconstruct as G {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef as R {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Read as R {- hsc3 -}

-- > let sy = "/home/rohan/sw/hsc3-graphs/scsyndef/why-supercollider.scsyndef"
-- > scsyndef_to_hs sy "/dev/stdout"
scsyndef_to_hs :: FilePath -> FilePath -> IO ()
scsyndef_to_hs sy_nm hs_nm = do
  gr <- R.read_graphdef_file sy_nm
  let nm = dropExtension (takeFileName sy_nm) -- ascii_to_string (R.graphdef_name gr)
      (_,gr') = R.graphdef_to_graph gr
      hs = G.reconstruct_graph_module nm gr'
  writeFile hs_nm (unlines hs)

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> scsyndef_to_hs "/dev/stdin" "/dev/stdout"
    [sy] -> scsyndef_to_hs sy "/dev/stdout"
    [sy,hs] -> scsyndef_to_hs sy hs
    _ -> putStrLn "scsyndef-to-hs scyndef [hs]"
