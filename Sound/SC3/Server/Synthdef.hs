-- | The unit-generator graph structure implemented by the
--   SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef where

import qualified Data.ByteString.Lazy as L {- bytestring -}
import Data.Default {- data-default -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.FilePath {- filepath -}

import qualified Sound.SC3.Server.Graphdef as G
import Sound.SC3.Server.Graph
import Sound.SC3.UGen.Help.Graph
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | A named unit generator graph.
data Synthdef = Synthdef {synthdefName :: String
                         ,synthdefUGen :: UGen}
                deriving (Eq,Show)

instance Default Synthdef where def = defaultSynthdef

-- | Lift a 'UGen' graph into a 'Synthdef'.
synthdef :: String -> UGen -> Synthdef
synthdef = Synthdef

-- | The SC3 /default/ instrument 'Synthdef', see
-- 'default_ugen_graph'.
--
-- > withSC3 (send (d_recv defaultSynthdef))
-- > audition defaultSynthdef
defaultSynthdef :: Synthdef
defaultSynthdef = synthdef "default" default_ugen_graph

-- | The SC3 /default/ sample (buffer) playback instrument 'Synthdef',
-- see 'default_sampler_ugen_graph'.
--
-- > withSC3 (send (d_recv (defaultSampler False)))
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
-- > synthdefParam def == ["amp","pan","gate","freq"]
synthdefParam :: Synthdef -> [String]
synthdefParam = map node_k_name . controls . synthdefGraph

-- | Find the indices of the named UGen at 'Graph'.  The index is
-- required when using 'Sound.SC3.Server.Command.u_cmd'.
ugenIndices :: String -> Graph -> [Integer]
ugenIndices nm =
    let f (k,nd) =
            case nd of
              NodeU _ _ nm' _ _ _ _ -> if nm == nm' then Just k else Nothing
              _ -> Nothing
    in mapMaybe f . zip [0..] . ugens

-- | 'graph_to_graphdef' at 'Synthdef'.
synthdef_to_graphdef :: Synthdef -> G.Graphdef
synthdef_to_graphdef (Synthdef nm u) = graph_to_graphdef nm (ugen_to_graph u)

-- | Encode 'Synthdef' as a binary data stream.
synthdefData :: Synthdef -> L.ByteString
synthdefData = G.encode_graphdef . synthdef_to_graphdef

-- | Write 'Synthdef' to indicated directory.  The filename is the
-- 'synthdefName' with the appropriate extension (@scsyndef@).
synthdefWrite :: Synthdef -> FilePath -> IO ()
synthdefWrite s dir =
    let nm = dir </> synthdefName s <.> "scsyndef"
    in L.writeFile nm (synthdefData s)

-- | Simple statistical analysis of a unit generator graph.
graph_stat :: Graph -> String
graph_stat s =
    let cs = constants s
        ks = controls s
        us = ugens s
        u_nm z = ugen_user_name (node_u_name z) (node_u_special z)
        f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "graph_stat"
              in show . map h . group . sort . map g
        sq = intercalate "," (map u_nm us)
    in unlines ["number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"control rates             : " ++ f node_k_rate ks
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f node_u_rate us
               ,"unit generator sequence   : " ++ sq]

-- | 'graph_stat' of 'synth'.
synthstat :: UGen -> String
synthstat = graph_stat . ugen_to_graph
