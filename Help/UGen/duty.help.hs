-- duty
uid_st_eval (
  do n0 <- drandM dinf (mce [0.01,0.2,0.4])
     n1 <- dseqM dinf (mce [204,400,201,502,300,200])
     let f = duty kr n0 0 RemoveSynth n1
     return (sinOsc ar (f * mce2 1 1.01) 0 * 0.1))

-- duty ; mouseX (control rate signal) determines duration
let n = dseqId 'α' dinf (mce [204,400,201,502,300,200])
    x = mouseX kr 0.001 2 Linear 0.1
    f = duty kr x 0 RemoveSynth n
in sinOsc ar (f * mce2 1 1.01) 0 * 0.1

-- duty ; fixed number of sample impulses ; https://fredrikolofsson.com/f0blog/impulse-train/
let dur = 1
    num = 8
in duty ar (dseqId 'α' dinf (mce [sampleDur, dur / num - sampleDur])) 0 DoNothing (dseqId 'β' num (mce [1, 0]))
