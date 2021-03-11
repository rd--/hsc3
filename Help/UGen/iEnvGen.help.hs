-- iEnvGen
let e = let l = [0,0.6,0.3,1.0,0]
            t = [0.1,0.02,0.4,1.1]
            c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
        in Envelope l t c Nothing Nothing 0
    x = mouseX KR 0 (envelope_duration e) Linear 0.2
    g = iEnvGen KR x e
in sinOsc AR (g * 500 + 440) 0 * 0.1

-- iEnvGen ; index with sinOsc ; mouse controls amplitude ; offset negative values of SinOsc
let e = let l = [-1,-0.7,0.7,1]
            t = [0.8666,0.2666,0.8668]
            c = [EnvLin,EnvLin]
        in Envelope l t c Nothing Nothing (-1)
    x = mouseX KR 0 1 Linear 0.2
    o = (sinOsc AR 440 0) * x
in iEnvGen AR o e * 0.1

-- iEnvGen ; index with amplitude of input ; control freq of SinOsc
let e = envXYC [(0, 330, EnvExp), (0.5, 440, EnvExp), (1.0, 1760, EnvLin)]
    pt = amplitude AR (soundIn 0) 0.01 0.2
in sinOsc AR (iEnvGen KR pt e) 0 * 0.2

-- iEnvGen ; using line to generate time signal ; ie. time can move faster and start near the end
let param = envPairs [(0,110),(4,220),(9,440),(11,220),(13,880),(19,110),(23,55),(27,55)] EnvExp
    (start,end,scale) = (11,27,1.5)
    time = line KR start end ((end - start) / scale) DoNothing
    freq = iEnvGen KR time param
    print_time = poll (impulse KR 1 0) time 0 (label "time")
    print_freq = poll (impulse KR 1 0) freq 0 (label "freq")
in mrg [pan2 (sinOsc AR 220 0 + sinOsc AR freq 0) 0 0.1,print_time,print_freq]
