module Sound.SC3.UGen.Netlist where

import System.IO.Unsafe {- base -}

import qualified Data.Reify as Reify {- data-reify -}

import Sound.SC3.Common.UId {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}

type Netlist = ([(Id,Circuit Id)], Id)

ugenNetlistIO :: UGen -> IO Netlist
ugenNetlistIO u = do
  Reify.Graph nl rt <- Reify.reifyGraph u
  return (nl,rt)

{-# NOINLINE ugenNetlist #-}
ugenNetlist :: UGen -> Netlist
ugenNetlist u = unsafePerformIO (ugenNetlistIO u)
