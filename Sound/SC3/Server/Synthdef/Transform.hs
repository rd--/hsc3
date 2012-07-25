-- | Transformations over 'Graph' structure.
module Sound.SC3.Server.Synthdef.Transform where

import Data.List
import Data.Maybe
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.Rate

-- * Lift constants

-- | Transform 'NodeC' to 'NodeK', 'id' for other 'Node' types.
--
-- > constant_to_control 8 (NodeC 0 0.1) == (NodeK 8 KR "k_8" 0.1 K_KR,9)
constant_to_control :: NodeId -> Node -> (NodeId,Node)
constant_to_control z n =
    case n of
      NodeC _ k -> (z+1,NodeK z KR ("k_" ++ show z) k K_KR)
      _ -> (z,n)

-- | Erroring variant of 'from_port_node'.
from_port_node_err :: Graph -> FromPort -> Node
from_port_node_err g fp =
    let e = error "from_port_node_err"
    in fromMaybe e (from_port_node g fp)

-- | Lift a set of 'NodeU' /inputs/ from constants to controls.  The
-- result triple gives the incremented 'NodeId', the transformed
-- 'FromPort' list, and the list of newly minted control 'Node's.
c_lift_inputs :: Graph -> NodeId -> [FromPort] -> (NodeId,[FromPort],[Node])
c_lift_inputs g z i =
    let n = map (from_port_node_err g) i
        (z',n') = mapAccumL constant_to_control z n
    in (z',map as_from_port n',filter is_node_k n')

c_lift_ugen :: Graph -> NodeId -> Node -> (NodeId,Node,[Node])
c_lift_ugen g z n =
    let i = node_u_inputs n
        (z',i',k) = c_lift_inputs g z i
    in (z',n {node_u_inputs = i'},k)

c_lift_ugens :: Graph -> NodeId -> [Node] -> ([Node],[Node])
c_lift_ugens g  =
    let rec (k,r) z u =
            case u of
              [] -> (k,reverse r)
              n:u' -> let (z',n',k') = c_lift_ugen g z n
                      in rec (k++k',n':r) z' u'
    in rec ([],[])

-- > import Sound.SC3
-- > import Sound.SC3.UGen.Dot
--
-- > let u = out 0 (sinOsc AR 440 0 * 0.1)
-- > let g = synth u
-- > draw g
-- > draw (lift_constants g)
lift_constants :: Graph -> Graph
lift_constants g =
    let (Graph _ _ k u) = remove_implicit g
        z = graph_maximum_id g + 1
        (k',u') = c_lift_ugens g z u
        g' = Graph (-1) [] (nub (k ++ k')) u'
    in add_implicit g'
