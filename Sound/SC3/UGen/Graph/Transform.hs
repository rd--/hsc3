-- | Transformations over 'Graph' structure.
module Sound.SC3.UGen.Graph.Transform where

import Data.Either {- base -}
import Data.List {- base -}

import Sound.SC3.UGen.Graph
import Sound.SC3.UGen.Rate

-- * Lift constants

-- | Transform 'Node_C' to 'Node_K', 'id' for other 'Node' types.
--
-- > let k = Node_K 8 KR Nothing "k_8" 0.1 K_KR Nothing
-- > node_k_eq k (snd (constant_to_control 8 (Node_C 0 0.1)))
constant_to_control :: Node_Id -> Node -> (Node_Id,Node)
constant_to_control z n =
    case n of
      Node_C _ k -> (z + 1,Node_K z KR Nothing ("k_" ++ show z) k K_KR Nothing)
      _ -> (z,n)

-- | If the 'From_Port' is a /constant/ generate a /control/ 'Node', else retain 'From_Port'.
c_lift_from_port :: Graph -> Node_Id -> From_Port -> (Node_Id,Either From_Port Node)
c_lift_from_port g z fp =
    case fp of
      From_Port_C _ ->
        let n = from_port_node_err g fp
            (z',n') = constant_to_control z n
        in (z',Right n')
      _ -> (z,Left fp)

-- | Lift a set of 'NodeU' /inputs/ from constants to controls.  The
-- result triple gives the incremented 'Node_Id', the transformed
-- 'From_Port' list, and the list of newly minted control 'Node's.
c_lift_inputs :: Graph -> Node_Id -> [From_Port] -> (Node_Id,[From_Port],[Node])
c_lift_inputs g z i =
    let (z',r) = mapAccumL (c_lift_from_port g) z i
        f e = case e of
                Left fp -> fp
                Right n -> as_from_port n
        r' = map f r
    in (z',r',rights r)

-- | Lift inputs at 'Node_U' as required.
c_lift_ugen :: Graph -> Node_Id -> Node -> (Node_Id,Node,[Node])
c_lift_ugen g z n =
    let i = node_u_inputs n
        (z',i',k) = c_lift_inputs g z i
    in (z',n {node_u_inputs = i'},k)

-- | 'c_lift_ugen' at list of 'Node_U'.
c_lift_ugens :: Graph -> Node_Id -> [Node] -> (Node_Id,[Node],[Node])
c_lift_ugens g  =
    let recur (k,r) z u =
            case u of
              [] -> (z,k,reverse r)
              n:u' -> let (z',n',k') = c_lift_ugen g z n
                      in recur (k++k',n':r) z' u'
    in recur ([],[])

{-| Lift constants to controls.

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Dot {- hsc3-dot -}

> let u = out 0 (sinOsc AR 440 0 * 0.1)
> let g = ugen_to_graph u
> draw g
> draw (lift_constants g)

-}
lift_constants :: Graph -> Graph
lift_constants g =
    let (Graph z _ k u) = remove_implicit g
        (z',k',u') = c_lift_ugens g z u
        g' = Graph z' [] (nubBy node_k_eq (k ++ k')) u'
    in add_implicit g'
