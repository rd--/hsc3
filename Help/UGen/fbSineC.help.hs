-- fbSineC ; SC3 default values
fbSineC ar (sampleRate / 2) 1 0.1 1.1 0.5 0.1 0.1 * 0.1

-- fbSineC ; fb generating noise
fbSineC ar (sampleRate / 2) 1 4 1.1 0.5 0.1 0.1 * 0.1

-- fbSineC ; increase feedback
let fb = line kr 0.01 4 10 DoNothing
in fbSineC ar sampleRate 1 fb 1.1 0.5 0.1 0.1 * 0.1

-- fbSineC ; increase phase multiplier
let a = line kr 1 2 10 DoNothing
in fbSineC ar sampleRate 1 0 a 0.5 0.1 0.1 * 0.1

-- fbSineC ; randomly modulate parameters
let x = mouseX kr 1 12 Linear 0.1
    nId e = lfNoise2Id e kr x
    n0 = nId 'α' * 10000 + 10000
    n1 = nId 'β' * 32 + 33
    n2 = nId 'γ' * 0.5
    n3 = nId 'δ' * 0.05 + 1.05
    n4 = nId 'ε' * 0.3 + 0.3
in fbSineC ar n0 n1 n2 n3 n4 0.1 0.1 * 0.1

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (600,1) 1.0 (fbSineC ar 600 1.0 4.0 1.1 0.5 0.1 0.1)

---- ; haskell implementation of equation
import Sound.SC3.Common.Math.Noise {- hsc3 -}
fbSineC_hs im fb a c = map fst (iterate (fbSine_f im fb a c) (0.1,0.1))
Sound.SC3.Plot.plot_p1_ln [take 600 (fbSineC_hs 1.0 4.0 1.1 0.5)]
