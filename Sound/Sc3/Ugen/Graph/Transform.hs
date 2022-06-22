-- | Transformations over 'Graph' structure.
module Sound.Sc3.Ugen.Graph.Transform where

import Data.Either {- base -}
import Data.List {- base -}

import qualified Sound.Sc3.Common.Rate as Rate {- hsc3 -}
import qualified Sound.Sc3.Common.Uid as Uid {- hsc3 -}
import Sound.Sc3.Ugen.Graph

-- * Lift constants

-- | Transform 'U_Node_C' to 'U_Node_K', 'id' for other 'U_Node' types.
--
-- > let k = U_Node_K 8 ControlRate Nothing "k_8" 0.1 K_ControlRate Nothing
-- > node_k_eq k (snd (constant_to_control 8 (U_Node_C 0 0.1)))
constant_to_control :: Uid.Id -> U_Node -> (Uid.Id,U_Node)
constant_to_control z n =
    case n of
      U_Node_C _ k -> (z + 1,U_Node_K z Rate.ControlRate Nothing ("k_" ++ show z) k Rate.K_ControlRate Nothing)
      _ -> (z,n)

-- | If the 'From_Port' is a /constant/ generate a /control/ 'U_Node', else retain 'From_Port'.
c_lift_from_port :: U_Graph -> Uid.Id -> From_Port -> (Uid.Id,Either From_Port U_Node)
c_lift_from_port g z fp =
    case fp of
      From_Port_C _ ->
        let n = ug_from_port_node_err g fp
            (z',n') = constant_to_control z n
        in (z',Right n')
      _ -> (z,Left fp)

-- | Lift a set of 'U_NodeU' /inputs/ from constants to controls.  The
-- result triple gives the incremented 'Uid.Id', the transformed
-- 'From_Port' list, and the list of newly minted control 'U_Node's.
c_lift_inputs :: U_Graph -> Uid.Id -> [From_Port] -> (Uid.Id,[From_Port],[U_Node])
c_lift_inputs g z i =
    let (z',r) = mapAccumL (c_lift_from_port g) z i
        f e = case e of
                Left fp -> fp
                Right n -> u_node_from_port n
        r' = map f r
    in (z',r',rights r)

-- | Lift inputs at 'U_Node_U' as required.
c_lift_ugen :: U_Graph -> Uid.Id -> U_Node -> (Uid.Id,U_Node,[U_Node])
c_lift_ugen g z n =
    let i = u_node_u_inputs n
        (z',i',k) = c_lift_inputs g z i
    in (z',n {u_node_u_inputs = i'},k)

-- | 'c_lift_ugen' at list of 'U_Node_U'.
c_lift_ugens :: U_Graph -> Uid.Id -> [U_Node] -> (Uid.Id,[U_Node],[U_Node])
c_lift_ugens g  =
    let recur (k,r) z u =
            case u of
              [] -> (z,k,reverse r)
              n:u' -> let (z',n',k') = c_lift_ugen g z n
                      in recur (k++k',n':r) z' u'
    in recur ([],[])

{-| Lift constants to controls.

> import Sound.Sc3 {- hsc3 -}
> import Sound.Sc3.Ugen.Dot {- hsc3-dot -}

> let u = out 0 (sinOsc AR 440 0 * 0.1)
> let g = ugen_to_graph u
> draw g
> draw (lift_constants g)

-}
lift_constants :: U_Graph -> U_Graph
lift_constants g =
    let (U_Graph z _ k u) = ug_remove_implicit g
        (z',k',u') = c_lift_ugens g z u
        g' = U_Graph z' [] (nubBy u_node_k_eq (k ++ k')) u'
    in ug_add_implicit g'
