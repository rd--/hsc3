-- | The unit-generator graph structure implemented by the SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef where

import qualified Data.ByteString.Lazy as L {- bytestring -}

import qualified Sound.SC3.UGen.Graph as UGen.Graph {- hsc3 -}
import qualified Sound.SC3.UGen.Help.Graph as Help.Graph {- hsc3 -}
import qualified Sound.SC3.UGen.Type as UGen {- hsc3 -}

import qualified Sound.SC3.Server.Graphdef as Graphdef {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Binary as Graphdef {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Graph as Graph {- hsc3 -}
import qualified Sound.SC3.Server.Param as Param {- hsc3 -}

-- | A named unit generator graph.
data Synthdef = Synthdef {synthdefName :: String
                         ,synthdefUGen :: UGen.UGen}
                deriving (Eq,Show)

-- | Alias for 'Synthdef'.
synthdef :: String -> UGen.UGen -> Synthdef
synthdef = Synthdef

{- | The SC3 /default/ instrument 'Synthdef', see 'default_ugen_graph'.

> import Sound.Osc {- hosc -}
> import Sound.SC3 {- hsc3 -}
> withSC3 (sendMessage (d_recv defaultSynthdef))
> audition defaultSynthdef

-}
defaultSynthdef :: Synthdef
defaultSynthdef = synthdef "default" Help.Graph.default_ugen_graph

-- | The SC3 /default/ sample (buffer) playback instrument 'Synthdef',
-- see 'default_sampler_ugen_graph'.
--
-- > withSC3 (sendMessage (d_recv (defaultSampler False)))
-- > audition (defaultSampler False)
defaultSampler :: Bool -> Synthdef
defaultSampler use_gate =
    let nm = "default-sampler-" ++ if use_gate then "gate" else "fixed"
    in synthdef nm (Help.Graph.default_sampler_ugen_graph use_gate)

-- | 'ugen_to_graph' of 'synthdefUGen'.
synthdefGraph :: Synthdef -> UGen.Graph.U_Graph
synthdefGraph = UGen.Graph.ugen_to_graph . synthdefUGen

-- | Parameter names at 'Synthdef'.
--
-- > synthdefParam defaultSynthdef == [("amp",0.1),("pan",0),("gate",1),("freq",440),("out",0)]
synthdefParam :: Synthdef -> Param.Param
synthdefParam =
  map (\n -> (UGen.Graph.u_node_k_name n,UGen.Graph.u_node_k_default n)) .
  UGen.Graph.ug_controls .
  synthdefGraph

-- | 'graph_to_graphdef' at 'Synthdef'.
synthdef_to_graphdef :: Synthdef -> Graphdef.Graphdef
synthdef_to_graphdef (Synthdef nm u) = Graph.graph_to_graphdef nm (UGen.Graph.ugen_to_graph u)

-- | 'graph_to_graphdef' at 'Synthdef'.
ugen_to_graphdef :: UGen.UGen -> Graphdef.Graphdef
ugen_to_graphdef = synthdef_to_graphdef . Synthdef "anonymous"

-- | Encode 'Synthdef' as a binary data stream.
synthdefData :: Synthdef -> L.ByteString
synthdefData = Graphdef.encode_graphdef . synthdef_to_graphdef

-- | Write 'Synthdef' to indicated file.
synthdefWrite :: FilePath -> Synthdef -> IO ()
synthdefWrite fn = Graphdef.graphdefWrite fn . synthdef_to_graphdef

-- | Write 'Synthdef' to indicated directory.  The filename is the
-- 'synthdefName' with the appropriate extension (@scsyndef@).
synthdefWrite_dir :: FilePath -> Synthdef -> IO ()
synthdefWrite_dir dir = Graphdef.graphdefWrite_dir dir . synthdef_to_graphdef

-- | 'graph_stat_ln' of 'synth'.
synthstat_ln :: UGen.UGen -> [String]
synthstat_ln = UGen.Graph.ug_stat_ln . UGen.Graph.ugen_to_graph

-- | 'unlines' of 'synthstat_ln'.
synthstat :: UGen.UGen -> String
synthstat = unlines . synthstat_ln

-- | 'putStrLn' of 'synthstat'.
--
-- > synthstat_wr Sound.SC3.UGen.Help.Graph.default_ugen_graph
synthstat_wr :: UGen.UGen -> IO ()
synthstat_wr = putStrLn . synthstat

-- | Variant without UGen sequence.
--
-- > putStrLn $ synthstat_concise (default_sampler_ugen_graph True)
synthstat_concise :: UGen.UGen -> String
synthstat_concise = unlines . reverse . drop 1 . reverse . synthstat_ln

-- | 'graphdef_dump_ugens' of 'ugen_to_graphdef'
ugen_dump_ugens :: UGen.UGen -> IO ()
ugen_dump_ugens = Graphdef.graphdef_dump_ugens . ugen_to_graphdef
