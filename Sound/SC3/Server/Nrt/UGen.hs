-- | Nrt from UGen
module Sound.SC3.Server.Nrt.UGen where

import Sound.OSC {- hosc -}

import Sound.SC3.Common.Rate
import Sound.SC3.Server.Command.Plain
import Sound.SC3.Server.Enum
import Sound.SC3.Server.Nrt
import Sound.SC3.Server.Nrt.Render
import Sound.SC3.Server.Synthdef
import Sound.SC3.UGen.Bindings.DB
import Sound.SC3.UGen.Type

{- | Make Nrt score that runs UGen for Time seconds to output bus zero.
     If UGen is at ControlRate insert 'k2a' UGen.
-}
nrt_ugen_rec :: Time -> UGen -> Nrt
nrt_ugen_rec dur u =
    let sg = case rateOf u of
               AudioRate -> u
               ControlRate -> k2a u
               _ -> error "nrt_ugen_rec: rate?"
        sy = synthdef "anonymous" (out 0 sg)
        m0 = d_recv sy
        m1 = s_new0 "anonymous" 1 AddToHead 0
    in Nrt [bundle 0 [m0,m1],bundle dur [nrt_end]]

{- | 'nrt_render_plain' of 'ugen_rec_nrt'.
     The number of channels is equal to the degree of the UGen.
-}
nrt_ugen_render :: (FilePath, FilePath, Int, SampleFormat, [String]) -> Time -> UGen -> IO ()
nrt_ugen_render (osc_fn,sf_fn,sample_rate,fmt,opt) dur u = do
  let sc = nrt_ugen_rec dur u
      nc = length (mceChannels u)
  nrt_render_plain (osc_fn,sf_fn,nc,sample_rate,fmt,opt) sc
