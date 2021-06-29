-- filter input (jmcc) #5 ; warning=feedback
let rQ = mouseY kr 0.01 1 Exponential 0.2 {- bandwidth ratio = 1/Q -}
    cf = mouseX kr 100 12000 Exponential 0.2 {- cutoff freq -}
    sg = soundIn (mce2 0 1) * 0.4 * sqrt rQ {- attenuate to offset resonance -}
in rlpf sg cf rQ
