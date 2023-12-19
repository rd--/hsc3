-- | Nrt from Ugen
module Sound.Sc3.Server.Nrt.Ugen where

import qualified Sound.Osc.Datum as Osc {- hosc -}
import qualified Sound.Osc.Packet as Osc {- hosc -}

import qualified Sound.Sc3.Common.Rate as Rate
import qualified Sound.Sc3.Server.Command.Plain as Server.Command
import qualified Sound.Sc3.Server.Enum as Server.Enum
import qualified Sound.Sc3.Server.Graphdef as Graphdef
import qualified Sound.Sc3.Server.Graphdef.Binary as Graphdef.Binary
import qualified Sound.Sc3.Server.Nrt as Nrt
import qualified Sound.Sc3.Server.Nrt.Render as Nrt.Render
import qualified Sound.Sc3.Server.Synthdef as Synthdef
import qualified Sound.Sc3.Ugen.Bindings.Db as Ugen.Binding
import qualified Sound.Sc3.Ugen.Ugen as Ugen

nrt_encoded_graphdef_rec :: Osc.Time -> Osc.Blob -> Nrt.Nrt
nrt_encoded_graphdef_rec dur graphdef =
  let m0 = Server.Command.d_recv_bytes graphdef
      m1 = Server.Command.s_new0 "Anonymous" 1 Server.Enum.AddToHead 0
  in Nrt.Nrt [Osc.bundle 0 [m0, m1], Osc.bundle dur [Server.Command.nrt_end]]

nrt_graphdef_rec :: Osc.Time -> Graphdef.Graphdef -> Nrt.Nrt
nrt_graphdef_rec dur graphdef = nrt_encoded_graphdef_rec dur (Graphdef.Binary.encode_graphdef graphdef)

nrt_syndef_rec :: Osc.Time -> Synthdef.Synthdef -> Nrt.Nrt
nrt_syndef_rec dur syndef = nrt_graphdef_rec dur (Synthdef.synthdef_to_graphdef syndef)

{- | Make Nrt score that runs Ugen for Time seconds to output bus zero.
If Ugen is at ControlRate insert 'k2a' Ugen.
-}
nrt_ugen_rec :: Osc.Time -> Ugen.Ugen -> Nrt.Nrt
nrt_ugen_rec dur ugen =
  let ugen' =
        case Ugen.rateOf ugen of
          Rate.AudioRate -> ugen
          Rate.ControlRate -> Ugen.Binding.k2a ugen
          _ -> error "nrt_ugen_rec: rate?"
  in nrt_syndef_rec dur (Synthdef.synthdef "Anonymous" (Ugen.Binding.out 0 ugen'))

-- | (osc-file, sound-file, sample-rate, sample-format, scsynth-options)
type Nrt_Opt = (FilePath, FilePath, Int, Server.Enum.SampleFormat, [String])

nrt_encoded_graphdef_render :: Nrt_Opt -> Osc.Time -> Int -> Osc.Blob -> IO ()
nrt_encoded_graphdef_render (osc_fn, sf_fn, sample_rate, fmt, opt) dur nc syndef = do
  let sc = nrt_encoded_graphdef_rec dur syndef
  Nrt.Render.nrt_render_plain (osc_fn, sf_fn, nc, sample_rate, fmt, opt) sc

nrt_syndef_render :: Nrt_Opt -> Osc.Time -> Int -> Synthdef.Synthdef -> IO ()
nrt_syndef_render (osc_fn, sf_fn, sample_rate, fmt, opt) dur nc syndef = do
  let sc = nrt_syndef_rec dur syndef
  Nrt.Render.nrt_render_plain (osc_fn, sf_fn, nc, sample_rate, fmt, opt) sc

{- | 'nrt_render_plain' of 'ugen_rec_nrt'.
The number of channels is equal to the degree of the Ugen.
-}
nrt_ugen_render :: Nrt_Opt -> Osc.Time -> Ugen.Ugen -> IO ()
nrt_ugen_render (osc_fn, sf_fn, sample_rate, fmt, opt) dur u = do
  let sc = nrt_ugen_rec dur u
      nc = length (Ugen.mceChannels u)
  Nrt.Render.nrt_render_plain (osc_fn, sf_fn, nc, sample_rate, fmt, opt) sc
