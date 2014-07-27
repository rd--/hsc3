-- | Transform (read) a 'Graphdef' into a 'Graph'.
module Sound.SC3.Server.Graphdef.Read where

import Sound.OSC.Type
import Sound.SC3.Server.Graphdef
import qualified Sound.SC3.UGen.Graph as G
import qualified Sound.SC3.UGen.Rate as R
import qualified Sound.SC3.UGen.Type as U

mk_node_k :: Graphdef -> G.NodeId -> (Control,U.Sample) -> G.Node
mk_node_k g z ((nm,ix),v) =
    let z' = graphdef_control_nid g z
        nm' = ascii_to_string nm
    in G.NodeK z' R.KR (Just ix) nm' v G.K_KR Nothing

input_to_from_port :: Graphdef -> Input -> G.FromPort
input_to_from_port g (Input u p) =
    if u == -1
    then G.FromPort_C (graphdef_constant_nid g p)
    else if input_is_control g (Input u p)
         then if u /= 0
              then error "multiple control UGens..."
              else G.FromPort_K (graphdef_control_nid g p) G.K_KR
         else let ugen = graphdef_ugens g !! u
                  port = if length (ugen_outputs ugen) > 1
                         then Just p
                         else Nothing
              in G.FromPort_U (graphdef_ugen_nid g u) port

mk_node_u :: Graphdef -> G.NodeId -> UGen -> G.Node
mk_node_u g z u =
    let (name,rate,inputs,outputs,special) = u
        z' = graphdef_ugen_nid g z
        rate' = toEnum rate
        name' = ascii_to_string name
        inputs' = map (input_to_from_port g) inputs
        outputs' = map toEnum outputs
        special' = U.Special special
    in G.NodeU z' rate' name' inputs' outputs' special' (U.UId z')

graphdef_to_graph :: Graphdef -> (String,G.Graph)
graphdef_to_graph g =
    let constants_nd = zipWith G.NodeC [0..] (graphdef_constants g)
        controls_nd = zipWith (mk_node_k g) [0 ..] (graphdef_controls g)
        ugens_nd = zipWith (mk_node_u g) [0 ..] (graphdef_ugens g)
        nm = ascii_to_string (graphdef_name g)
        gr = G.Graph (-1) constants_nd controls_nd ugens_nd
    in (nm,gr) -- S.Synthdef nm gr
