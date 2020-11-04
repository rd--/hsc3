-- runningSum ; distorts of course, would need scaling
runningSum (soundIn 0 * 0.1) 40

-- runningSum ; running average over x samples
let x = 100 in runningSum (lfSaw AR 440 0 * 0.1) x * recip x

-- runningSum ; rms power
let input = lfSaw AR 440 0 * 0.1
    numsamp = 30
in runningSum (input * input) numsamp / (sqrt numsamp)

-- runningSum ; composite UGen
runningSumRMS (soundIn 0) 40 * 0.1

-- runningSum
let z = soundIn 0
    a = runningSum z 40
in sinOsc AR 440 0 * a * 0.1
