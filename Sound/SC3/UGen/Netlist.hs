module Sound.SC3.UGen.Netlist where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.IO.Unsafe {- base -}

import qualified Data.Reify as Reify {- data-reify -}
import qualified Data.Reify.Graph.CSE as CSE {- data-reify-cse -}

import Sound.SC3.Common.UId {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}

type Netlist = ([(Id,Circuit Id)], Id)

ugenNetlistIO :: UGen -> IO Netlist
ugenNetlistIO u = do
  g <- Reify.reifyGraph u
  let Reify.Graph nl rt = CSE.cse g
  return (nl,rt)

{-# NOINLINE ugenNetlist #-}
ugenNetlist :: UGen -> Netlist
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
