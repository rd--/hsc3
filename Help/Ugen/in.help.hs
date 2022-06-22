-- in ; patching input to output (see also soundIn) ; in haskellId 'in' is a reserved keyword
in' 2 ar numOutputBuses

-- in ; patching input to output, with delay
let i = in' 2 ar numOutputBuses
    d = delayN i 0.5 0.5
in i + d

-- in ; write noise to first private bus, then read it out ; the multiple root graph is ordered
let n = pinkNoiseId 'α' ar * 0.1
    b = numOutputBuses + numInputBuses
    wr = out b n
    rd = out 0 (in' 1 ar b)
in mrg [rd,wr]

-- in ; there are functions to encapsulate the offset calculation, c.f. firstPrivateBus
let n = pinkNoiseId 'α' ar
    wr = privateOut 0 (n * 0.1)
    rd = out 0 (privateIn 1 ar 0)
in mrg [rd,wr]

-- in ; read control bus
let b = control kr "bus" 0
in sinOsc ar (in' 1 kr b) 0 * 0.1

-- in ; audio graph reading control buses 0 & 1 ; control graph writing buses
let wr = out 0 (mce2 (tRandId 'α' 220 2200 (dustId 'β' kr 1)) (dustId 'γ' kr 3))
in mrg2 (sinOsc ar (in' 1 kr 0) 0 * decay (in' 1 kr 1) 0.2 * 0.1) wr

-- in ; patch mono input to stereo output
pan2 (in' 1 ar numOutputBuses) 0 1

-- in ; make a control rate graph to write freq and gate values
let wr = out 10 (mce2 (tRandId 'α' 220 2200 (dustId 'β' kr 1)) (dustId 'γ' kr 3))
    rd = sinOsc ar (in' 1 kr 10) 0 * decay2 (in' 1 kr 11) 0.01 1 * 0.25
in mrg2 rd wr

---- ; set value on a control bus
withSc3 (Sound.OSC.sendMessage (c_set1 0 300))
withSc3 (Sound.OSC.sendMessage (c_set1 0 600))
