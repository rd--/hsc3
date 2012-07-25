-- | Transformations over 'Graph' structure.
module Sound.SC3.Server.Synthdef.Transform where

import Data.Either
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

-- | If the 'FromPort' is a /constant/ generate a /control/ 'Node',
-- else retain 'FromPort'.
c_lift_from_port :: Graph -> NodeId -> FromPort -> (NodeId,Either FromPort Node)
c_lift_from_port g z fp =
    case fp of
      FromPort_C _ -> let n = from_port_node_err g fp
                          (z',n') = constant_to_control z n
                      in (z',Right n')
      _ -> (z,Left fp)

-- | Lift a set of 'NodeU' /inputs/ from constants to controls.  The
-- result triple gives the incremented 'NodeId', the transformed
-- 'FromPort' list, and the list of newly minted control 'Node's.
c_lift_inputs :: Graph -> NodeId -> [FromPort] -> (NodeId,[FromPort],[Node])
c_lift_inputs g z i =
    let (z',r) = mapAccumL (c_lift_from_port g) z i
        f e = case e of
                Left fp -> fp
                Right n -> as_from_port n
        r' = map f r
    in (z',r',rights r)

c_lift_ugen :: Graph -> NodeId -> Node -> (NodeId,Node,[Node])
c_lift_ugen g z n =
    let i = node_u_inputs n
        (z',i',k) = c_lift_inputs g z i
    in (z',n {node_u_inputs = i'},k)

c_lift_ugens :: Graph -> NodeId -> [Node] -> (NodeId,[Node],[Node])
c_lift_ugens g  =
    let rec (k,r) z u =
            case u of
              [] -> (z,k,reverse r)
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
    let (Graph z _ k u) = remove_implicit g
        (z',k',u') = c_lift_ugens g z u
        g' = Graph z' [] (nub (k ++ k')) u'
    in add_implicit g'
