-- envGate ; default arguments, as used by envGate_def
let k = control kr
    e = envGate 1 (k "gate" 1) (k "fadeTime" 0.02) RemoveSynth EnvSin
in lpf (saw ar 200) 600 * 0.1 * e

-- envGate ; defaults arguments
let e = envGate_def
in lpf (saw ar 200) 600 * 0.1 * e

-- envGate ; several envGate nodes can coexist, if they are the same they are shared
let e = envGate_def
    s1 = lpf (saw ar 80) 600 * e
    s2 = rlpf (saw ar 200 * 0.5) (6000 * e + 60) 0.1 * e
in mce2 s1 s2 * 0.1

---- ; set fade time, then release gate
import Sound.OSC {- hosc -}
withSc3 (sendMessage (n_set1 (-1) "fadeTime" 5))
withSc3 (sendMessage (n_set1 (-1) "gate" 0))

