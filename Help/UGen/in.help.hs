-- in ; patching input to output (see also soundIn) ; in haskell 'in' is a reserved keyword
in' 2 AR numOutputBuses

-- in ; patching input to output, with delay
let i = in' 2 AR numOutputBuses
    d = delayN i 0.5 0.5
in i + d

-- in ; write noise to first private bus, then read it out ; the multiple root graph is ordered
let n = pinkNoise 'α' AR * 0.1
    b = numOutputBuses + numInputBuses
    wr = out b n
    rd = out 0 (in' 1 AR b)
in mrg [rd,wr]

-- in ; there are functions to encapsulate the offset calculation, c.f. firstPrivateBus
let n = pinkNoise 'α' AR
    wr = privateOut 0 (n * 0.1)
    rd = out 0 (privateIn 1 AR 0)
in mrg [rd,wr]

-- in ; read control bus
let b = control KR "bus" 0
in sinOsc AR (in' 1 KR b) 0 * 0.1

-- in ; audio graph reading control buses 0 & 1 ; control graph writing buses
let wr = out 0 (mce2 (tRand 'α' 220 2200 (dust 'β' KR 1)) (dust 'γ' KR 3))
in mrg2 (sinOsc AR (in' 1 KR 0) 0 * decay (in' 1 KR 1) 0.2 * 0.1) wr

-- in ; patch mono input to stereo output
pan2 (in' 1 AR numOutputBuses) 0 1

-- in ; make a control rate graph to write freq and gate values
let wr = out 10 (mce2 (tRand 'α' 220 2200 (dust 'β' KR 1)) (dust 'γ' KR 3))
    rd = sinOsc AR (in' 1 KR 10) 0 * decay2 (in' 1 KR 11) 0.01 1 * 0.25
in mrg2 rd wr

---- ; set value on a control bus
withSC3 (Sound.OSC.sendMessage (c_set1 0 300))
withSC3 (Sound.OSC.sendMessage (c_set1 0 600))
