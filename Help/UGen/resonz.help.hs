-- resonz
resonz (whiteNoise ar * 0.5) 2000 0.1

-- resonz ; modulate frequency
let f = xLine kr 1000 8000 10 RemoveSynth
in resonz (whiteNoise ar * 0.5) f 0.05

-- resonz ; modulate bandwidth
let bw = xLine kr 1 0.001 8 RemoveSynth
in resonz (whiteNoise ar * 0.5) 2000 bw

-- resonz ; modulate bandwidth opposite direction
let bw = xLine kr 0.001 1 8 RemoveSynth
in resonz (whiteNoise ar * 0.5) 2000 bw

-- resonz ; mouse control (1/Q = bandwidth / center-frequency)
let m = mouseX kr 36 85 Linear 0.2 {- midi note -}
    w = mouseY kr 0.1 5 Linear 0.2 {- bandwidth -}
    f = midiCps (floorE m) {- centre frequency -}
    rq = w / f {- 1/Q (reciprocal of Q) -}
in resonz (pinkNoise ar) f rq

-- resonz ; pinkNoise ; event control
let f (_,g,_,y,z,o,_,_,p,_,_) =
      pan2 (resonz (pinkNoise ar) (midiCps p) (y * 0.25) * 24) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- resonz ; pinkNoise ; event control
let f (_,g,_,y,z,o,rx,_,p,_,_) =
      let (gt,_) = eventGateReset g p
          e = envGen kr gt 1 0 1 DoNothing (envPerc 0.01 (1 + rx))
          f = midiCps p {- centre frequency -}
          rq = linLin y 0 1 0.05 0.25 / f {- 1/Q (reciprocal of Q) -}
          scl = 900
      in pan2 (resonz (pinkNoise ar) f rq * scl * z) (o * 2 - 1) e
in mix (eventVoicer 16 f) * control kr "gain" 1

{---- ; Q
The Q factor of a resonator is defined as the center-frequency (cf) divided by the bandwidth (bw).

> let q cf bw = cf / bw

The higher the Q the narrower the filter.

> let bw_set = [1,4,10,100,200,400]
> map (q 400) bw_set == [400,100,40,4,2,1]

Q multipled by the band-width gives the center-frequency.

> map (\bw -> q 400 bw * bw) bw_set == replicate (length bw_set) 400

The third argument to resonz is the reciprocal of Q (rq).

> let rq cf = recip . q cf
> map (rq 400) [1,4,10,100,200,400] == [1/400,1/100,1/40,1/4,1/2,1]

1/Q multiplied by the center-frequency gives the bandwidth.

> map (\bw -> rq 400 bw * 400) bw_set == bw_set
-}
