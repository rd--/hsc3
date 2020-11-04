-- numBuffers ; the number of audio buffers available at the server (by default 1024, printing only)
poll (impulse KR 1 0) numBuffers 0 (label "numBuffers")

-- numBuffers
let f = 110 + numBuffers in sinOsc AR f 0 * 0.1
