-- | Decode (read) a 'Graphdef' into a 'Graph'.
module Sound.Sc3.Server.Graphdef.Read where

import qualified Sound.Osc.Datum as Datum {- hosc -}

import qualified Sound.Sc3.Common.Rate as Rate {- hsc3 -}
import qualified Sound.Sc3.Common.Uid as Uid {- hsc3 -}
import qualified Sound.Sc3.Server.Graphdef as Graphdef {- hsc3 -}
import qualified Sound.Sc3.Server.Graphdef.Binary as Graphdef {- hsc3 -}
import qualified Sound.Sc3.Ugen.Graph as Graph {- hsc3 -}
import qualified Sound.Sc3.Ugen.Types as Types {- hsc3 -}

control_to_node :: Graphdef.Graphdef -> Uid.Id -> (Graphdef.Control,Types.Sample) -> Graph.U_Node
control_to_node g z ((nm,ix),v) =
    let z' = Graphdef.graphdef_control_nid g z
        nm' = Datum.ascii_to_string nm
    in Graph.U_Node_K z' Rate.ControlRate (Just ix) nm' v Rate.K_ControlRate Nothing

-- | Note: Graphs with multiple Control Ugens are not accounted for.
input_to_from_port :: Graphdef.Graphdef -> Graphdef.Input -> Graph.From_Port
input_to_from_port g (Graphdef.Input u p) =
    if u == -1
    then Graph.From_Port_C (Graphdef.graphdef_constant_nid g p)
    else if Graphdef.input_is_control g (Graphdef.Input u p)
         then Graph.From_Port_K (Graphdef.graphdef_control_nid g p) Rate.K_ControlRate
         else let ugen = Graphdef.graphdef_ugens g !! u
                  port = if length (Graphdef.ugen_outputs ugen) > 1
                         then Just p
                         else Nothing
              in Graph.From_Port_U (Graphdef.graphdef_ugen_nid g u) port

ugen_to_node :: Graphdef.Graphdef -> Uid.Id -> Graphdef.Ugen -> Graph.U_Node
ugen_to_node g z u =
    let (name,rate,inputs,outputs,special) = u
        z' = Graphdef.graphdef_ugen_nid g z
        rate' = toEnum rate
        name' = Datum.ascii_to_string name
        inputs' = map (input_to_from_port g) inputs
        outputs' = map toEnum outputs
        special' = Types.Special special
    in Graph.U_Node_U z' rate' name' inputs' outputs' special' (Types.Uid z')

graphdef_to_graph :: Graphdef.Graphdef -> (String,Graph.U_Graph)
graphdef_to_graph g =
    let constants_nd = zipWith Graph.U_Node_C [0..] (Graphdef.graphdef_constants g)
        controls_nd = zipWith (control_to_node g) [0 ..] (Graphdef.graphdef_controls g)
        ugens_nd = zipWith (ugen_to_node g) [0 ..] (Graphdef.graphdef_ugens g)
        nm = Datum.ascii_to_string (Graphdef.graphdef_name g)
        gr = Graph.U_Graph (-1) constants_nd controls_nd ugens_nd
    in (nm,gr) -- S.Synthdef nm gr

-- | Read graphdef file and translate to graph.
read_graph :: FilePath -> IO Graph.U_Graph
read_graph sy_nm = do
  d <- Graphdef.read_graphdef_file sy_nm
  let (_,g) = graphdef_to_graph d
  return g

-- | Read graphdef file, translate to graph, and run 'ug_stat_ln'.
scsyndef_ug_stat :: FilePath -> IO String
scsyndef_ug_stat sy_nm = do
  g <- read_graph sy_nm
  return (unlines (Graph.ug_stat_ln g))
