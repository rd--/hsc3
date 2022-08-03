-- iEnvGen
let e = let l = [0,0.6,0.3,1.0,0]
            t = [0.1,0.02,0.4,1.1]
            c = [EnvLin,EnvExp,EnvNum (-6),EnvSin]
        in Envelope l t c Nothing Nothing 0
    x = mouseX kr 0 (envelope_duration e) Linear 0.2
    g = iEnvGen kr x e
in sinOsc ar (g * 500 + 440) 0 * 0.1

-- iEnvGen ; index with sinOsc ; mouse controls amplitude ; offset negative values of SinOsc
let e = let l = [-1,-0.7,0.7,1]
            t = [0.8666,0.2666,0.8668]
            c = [EnvLin,EnvLin]
        in Envelope l t c Nothing Nothing (-1)
    x = mouseX kr 0 1 Linear 0.2
    o = (sinOsc ar 440 0) * x
in iEnvGen ar o e * 0.1

-- iEnvGen ; index with amplitude of input ; control freq of SinOsc
let e = envXyc [(0, 330, EnvExp), (0.5, 440, EnvExp), (1.0, 1760, EnvLin)]
    pt = amplitude ar (soundIn 0) 0.01 0.2
in sinOsc ar (iEnvGen kr pt e) 0 * 0.2

-- iEnvGen ; using line to generate time signal ; ie. time can move faster and start near the end
let param = envPairs [(0,110),(4,220),(9,440),(11,220),(13,880),(19,110),(23,55),(27,55)] EnvExp
    (start,end,scale) = (11,27,1.5)
    time = line kr start end ((end - start) / scale) DoNothing
    freq = iEnvGen kr time param
    print_time = poll (impulse kr 1 0) time 0 (label "time")
    print_freq = poll (impulse kr 1 0) freq 0 (label "freq")
in mrg [pan2 (sinOsc ar 220 0 + sinOsc ar freq 0) 0 0.1,print_time,print_freq]
