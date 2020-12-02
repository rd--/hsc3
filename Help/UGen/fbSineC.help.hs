-- fbSineC ; SC3 default values
fbSineC AR (sampleRate / 2) 1 0.1 1.1 0.5 0.1 0.1 * 0.1

-- fbSineC ; fb generating noise
fbSineC AR (sampleRate / 2) 1 4 1.1 0.5 0.1 0.1 * 0.1

-- fbSineC ; increase feedback
let fb = line KR 0.01 4 10 DoNothing
in fbSineC AR sampleRate 1 fb 1.1 0.5 0.1 0.1 * 0.1

-- fbSineC ; increase phase multiplier
let a = line KR 1 2 10 DoNothing
in fbSineC AR sampleRate 1 0 a 0.5 0.1 0.1 * 0.1

-- fbSineC ; randomly modulate parameters
let x = mouseX KR 1 12 Linear 0.1
    n e = lfNoise2 e KR x
    n0 = n 'α' * 10000 + 10000
    n1 = n 'β' * 32 + 33
    n2 = n 'γ' * 0.5
    n3 = n 'δ' * 0.05 + 1.05
    n4 = n 'ε' * 0.3 + 0.3
in fbSineC AR n0 n1 n2 n3 n4 0.1 0.1 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (600,1) 1.0 (fbSineC AR 600 1.0 4.0 1.1 0.5 0.1 0.1)

---- ; haskell implementation of equation
import Sound.SC3.Common.Math.Noise {- hsc3 -}
fbSineC_hs im fb a c = map fst (iterate (fbSine_f im fb a c) (0.1,0.1))
Sound.SC3.Plot.plot_p1_ln [take 600 (fbSineC_hs 1.0 4.0 1.1 0.5)]
