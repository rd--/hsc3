-- | Nrt from Ugen
module Sound.Sc3.Server.Nrt.Ugen where

import qualified Sound.Osc.Datum as Osc {- hosc -}
import qualified Sound.Osc.Packet as Osc {- hosc -}

import Sound.Sc3.Common.Rate
import Sound.Sc3.Server.Command.Plain
import Sound.Sc3.Server.Enum
import Sound.Sc3.Server.Nrt
import Sound.Sc3.Server.Nrt.Render
import Sound.Sc3.Server.Synthdef
import Sound.Sc3.Ugen.Bindings.Db
import Sound.Sc3.Ugen.Type

{- | Make Nrt score that runs Ugen for Time seconds to output bus zero.
     If Ugen is at ControlRate insert 'k2a' Ugen.
-}
nrt_ugen_rec :: Osc.Time -> Ugen -> Nrt
nrt_ugen_rec dur u =
    let sg = case rateOf u of
               AudioRate -> u
               ControlRate -> k2a u
               _ -> error "nrt_ugen_rec: rate?"
        sy = synthdef "anonymous" (out 0 sg)
        m0 = d_recv sy
        m1 = s_new0 "anonymous" 1 AddToHead 0
    in Nrt [Osc.bundle 0 [m0, m1], Osc.bundle dur [nrt_end]]

{- | 'nrt_render_plain' of 'ugen_rec_nrt'.
     The number of channels is equal to the degree of the Ugen.
-}
nrt_ugen_render :: (FilePath, FilePath, Int, SampleFormat, [String]) -> Osc.Time -> Ugen -> IO ()
nrt_ugen_render (osc_fn,sf_fn,sample_rate,fmt,opt) dur u = do
  let sc = nrt_ugen_rec dur u
      nc = length (mceChannels u)
  nrt_render_plain (osc_fn,sf_fn,nc,sample_rate,fmt,opt) sc
