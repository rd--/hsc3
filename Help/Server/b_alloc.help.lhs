> Sound.SC3.Server.Help.viewServerHelp "/b_alloc"

Buffer indices are not restricted by the number of available buffers
at the server.  Below allocates a buffer at index 8192.

> withSC3 (async (b_alloc_setn1 8192 0 [0,3,7,10]))

> let {x = mouseX KR 0 9 Linear 0.1
>     ;k = degreeToKey 8192 x 12
>     ;o = sinOsc AR (midiCPS (48 + k)) 0 * 0.1}
> in audition (out 0 o)
