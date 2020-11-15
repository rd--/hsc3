-- replaceOut
replaceOut 0 (sinOsc AR 440 0 * 0.1)

-- replaceOut ; send signal to a bus, overwrite existing signal
--            ; mrg nodes are ordered right to left, so below b replaces c and then a is summed
let a = sinOsc AR (mce [120, 121]) 0 * 0.1
    b = sinOsc AR (mce [330, 331]) 0 * 0.1
    c = sinOsc AR (mce [880, 881]) 0 * 0.1
in mrg [out 0 a, replaceOut 0 b, out 0 c]

-- replaceOut ; a writes noise to bus 24, b reads 24 and replaces with filtered variant, c reads 24 and writes to 0
let a = pinkNoise 'α' AR * 0.1
    b = bpf (in' 1 AR 24) 440 1
    c = in' 1 AR 24
in mrg [out 0 c, replaceOut 24 b, out 24 a]