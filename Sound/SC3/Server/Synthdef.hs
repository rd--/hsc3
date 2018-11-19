-- | The unit-generator graph structure implemented by the SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef where

import qualified Data.ByteString.Lazy as L {- bytestring -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}

import Sound.SC3.UGen.Graph
import Sound.SC3.UGen.Help.Graph
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

import qualified Sound.SC3.Server.Graphdef as Graphdef
import qualified Sound.SC3.Server.Graphdef.Graph as Graph

-- | A named unit generator graph.
data Synthdef = Synthdef {synthdefName :: String
                         ,synthdefUGen :: UGen}
                deriving (Eq,Show)

-- | Lift a 'UGen' graph into a 'Synthdef'.
synthdef :: String -> UGen -> Synthdef
synthdef = Synthdef

-- | The SC3 /default/ instrument 'Synthdef', see
-- 'default_ugen_graph'.
--
-- > import Sound.OSC {- hosc -}
-- > import Sound.SC3 {- hsc3 -}
-- > withSC3 (sendMessage (d_recv defaultSynthdef))
-- > audition defaultSynthdef
defaultSynthdef :: Synthdef
defaultSynthdef = synthdef "default" default_ugen_graph

-- | The SC3 /default/ sample (buffer) playback instrument 'Synthdef',
-- see 'default_sampler_ugen_graph'.
--
-- > withSC3 (sendMessage (d_recv (defaultSampler False)))
-- > audition (defaultSampler False)
defaultSampler :: Bool -> Synthdef
defaultSampler use_gate =
    let nm = "default-sampler-" ++ if use_gate then "gate" else "fixed"
    in synthdef nm (default_sampler_ugen_graph use_gate)

-- | 'ugen_to_graph' of 'synthdefUGen'.
synthdefGraph :: Synthdef -> Graph
synthdefGraph = ugen_to_graph . synthdefUGen

-- | Parameter names at 'Synthdef'.
--
-- > synthdefParam defaultSynthdef == ["amp","pan","gate","freq","out"]
synthdefParam :: Synthdef -> [String]
synthdefParam = map node_k_name . controls . synthdefGraph

-- | Find the indices of the named UGen at 'Graph'.  The index is
-- required when using 'Sound.SC3.Server.Command.u_cmd'.
ugenIndices :: String -> Graph -> [Integer]
ugenIndices nm =
    let f (k,nd) =
            case nd of
              Node_U _ _ nm' _ _ _ _ -> if nm == nm' then Just k else Nothing
              _ -> Nothing
    in mapMaybe f . zip [0..] . ugens

-- | 'graph_to_graphdef' at 'Synthdef'.
synthdef_to_graphdef :: Synthdef -> Graphdef.Graphdef
synthdef_to_graphdef (Synthdef nm u) = Graph.graph_to_graphdef nm (ugen_to_graph u)

-- | Encode 'Synthdef' as a binary data stream.
synthdefData :: Synthdef -> L.ByteString
synthdefData = Graphdef.encode_graphdef . synthdef_to_graphdef

-- | Write 'Synthdef' to indicated directory.  The filename is the
-- 'synthdefName' with the appropriate extension (@scsyndef@).
synthdefWrite :: Synthdef -> FilePath -> IO ()
synthdefWrite s dir =
    let nm = dir </> synthdefName s <.> "scsyndef"
    in L.writeFile nm (synthdefData s)

-- | Simple statistical analysis of a unit generator graph.
graph_stat_ln :: Graph -> [String]
graph_stat_ln s =
    let cs = constants s
        ks = controls s
        us = ugens s
        u_nm z = ugen_user_name (node_u_name z) (node_u_special z)
        hist pp_f =
          let h (x:xs) = (x,length (x:xs))
              h [] = error "graph_stat_ln"
          in unwords . map (\(p,q) -> pp_f p ++ "×" ++ show q) . map h . group . sort
    in ["number of constants       : " ++ show (length cs)
       ,"number of controls        : " ++ show (length ks)
       ,"control rates             : " ++ hist show (map node_k_rate ks)
       ,"control names             : " ++ unwords (map node_k_name ks)
       ,"number of unit generators : " ++ show (length us)
       ,"unit generator rates      : " ++ hist show (map node_u_rate us)
       ,"unit generator set        : " ++ hist id (map u_nm us)
       ,"unit generator sequence   : " ++ unwords (map u_nm us)]

-- | 'graph_stat_ln' of 'synth'.
synthstat_ln :: UGen -> [String]
synthstat_ln = graph_stat_ln . ugen_to_graph

-- | 'unlines' of 'synthstat_ln'.
--
-- > putStrLn $ synthstat Sound.SC3.UGen.Help.Graph.default_ugen_graph
synthstat :: UGen -> String
synthstat = unlines . synthstat_ln

-- | Variant without UGen sequence.
--
-- > putStrLn $ synthstat_concise (default_sampler_ugen_graph True)
synthstat_concise :: UGen -> String
synthstat_concise = unlines . reverse . drop 1 . reverse . synthstat_ln
