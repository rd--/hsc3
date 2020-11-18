-- envLinen
let t = envLinen 0.4 2 0.4 0.1
    e = envGen KR 1 1 0 1 RemoveSynth t
in sinOsc AR 440 0 * e

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plotEnvelope [envLinen 0.4 2 0.4 0.6,envLinen 0.6 1 1.2 0.7]
plotEnvelope [envLinen 0 1 0 0.4]
plotEnvelope [envelope_normalise (envLinen 0 2 0 0.5)]

---- ; help
Sound.SC3.UGen.Help.viewSC3Help "Env.*linen"
:i Sound.SC3.LINEN

---- ; language access
let e = envLinen 0 1 0 1 :: Envelope Double
in (envelope_duration e
   ,envelope_segment_ix e 0
   ,envelope_segment_ix e 1
   ,envelope_segment e 0
   ,envelope_segment e 1
   ,envelope_at e 0
   ,envelope_at e 1
   ,envelope_render 10 e)

