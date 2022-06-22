module Sound.Sc3.Ugen.Netlist where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.IO.Unsafe {- base -}

import qualified Data.Reify as Reify {- data-reify -}
import qualified Data.Reify.Graph.CSE as CSE {- data-reify-cse -}

import Sound.Sc3.Common.UId {- hsc3 -}
import Sound.Sc3.Ugen.Type {- hsc3 -}

type Netlist = ([(Id,Circuit Id)], Id)

ugenNetlistIO :: Ugen -> IO Netlist
ugenNetlistIO u = do
  g <- Reify.reifyGraph u
  --let Reify.Graph nl rt = CSE.cse g
  let Reify.Graph nl rt = g
  return (nl,rt)

{-# NOINLINE ugenNetlist #-}
ugenNetlist :: Ugen -> Netlist
ugenNetlist u = unsafePerformIO (ugenNetlistIO u)

netlistLookupCircuit :: Netlist -> Id -> Maybe (Circuit Id)
netlistLookupCircuit (l,_) k = lookup k l

netlistLookupCircuitNote :: String -> Netlist -> Id -> Circuit Id
netlistLookupCircuitNote msg nl = fromMaybe (error ("netlistLookupCircuit: " ++ msg)) . netlistLookupCircuit nl

netlistLookupId :: Netlist -> Circuit Id -> Maybe Id
netlistLookupId (l,_) c =
  let reverseLookup k = fmap fst . find ((== k) . snd)
  in reverseLookup c l

netlistLookupIdNote :: String -> Netlist -> Circuit Id -> Id
netlistLookupIdNote msg nl = fromMaybe (error ("netlistLookupId: " ++ msg)) . netlistLookupId nl


{-

-- * Netlist to U_Graph

import Sound.Sc3.Ugen.Netlist {- hsc3 -}

circuit_to_from_port :: Uid.Id -> Circuit Uid.Id -> From_Port
circuit_to_from_port k c =
  case c of
    CConstant _ -> From_Port_C k
    CControl (Control rt _ _ _ tr _) -> From_Port_K k (Rate.ktype rt tr)
    CPrimitive _ -> From_Port_U k Nothing
    CProxy (Proxy src ix _) -> From_Port_U src (Just ix)
    _ -> error "circuit_to_from_port?"

netlist_entry_to_u_node :: Netlist -> (Uid.Id,Circuit Uid.Id) -> Maybe U_Node
netlist_entry_to_u_node nl (k,c) =
    case c of
      CConstant (Constant x) -> Just (U_Node_C k x)
      CControl (Control r ix nm d tr meta) -> Just (U_Node_K k r ix nm d (Rate.ktype r tr) meta)
      CLabel _ -> error (show ("netlist_entry_to_node: label",k,c))
      CPrimitive (Primitive r nm is o s d) -> Just (U_Node_U k r nm (map (\i -> circuit_to_from_port i (netlistLookupCircuitNote "netlist_entry_to_u_node/Primitive" nl i)) is) o s d)
      CProxy (Proxy src ix rt) -> Just (U_Node_P k src ix rt)
      CMrg _ _ -> Nothing
      CMce _ _ -> Nothing -- error (show ("netlist_entry_to_node: mce",k,c))

netlist_to_u_graph :: Netlist -> U_Graph
netlist_to_u_graph (l,r) =
  let n = mapMaybe (netlist_entry_to_u_node (l,r)) l
  in U_Graph (maximum (map fst l) + 1) (filter u_node_is_c n) (filter u_node_is_k n) (filter u_node_is_u n)

-}
