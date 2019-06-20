import Sound.OSC {- hosc3 -}
import Sound.SC3 {- hsc3 -}

sy = synthdef "sin" (out 0 (sinOsc AR (control KR "freq" 440) 0 * 0.2))
b0 = bundle 0.0 [g_new [(1, AddToTail, 0)], d_recv sy]
b1 = bundle 0.5 [s_new "sin" 1001 AddToHead 1 []]
b2 = bundle 3.0 [n_free [1001]]
b3 = bundle 3.5 [nrt_end]

main = do
  writeNRT "/tmp/nrt.02.osc" (NRT [b0,b1,b2,b3])
  nrt_exec_plain ("/tmp/nrt.02.osc",("_",0),("/tmp/nrt.02.wav",1),48000,PcmInt16,[])
