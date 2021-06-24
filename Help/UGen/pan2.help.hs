-- pan2
let n = pinkNoise 'α' AR in pan2 n (fSinOsc KR 2 0) 0.1

-- pan2
let n = pinkNoise 'α' AR * 0.1
    x = mouseX KR (-1) 1 Linear 0.2
    y = mouseY KR 0 1 Linear 0.2
in pan2 n x y

-- pan2 ; ctl=freq:220,220,440,exp;amp:0.1,0,1,amp;pan:0,-1,1,lin
pan2 (sinOsc AR (control KR "freq" 220) 0) (control KR "pan" 0) (control KR "amp" 0.1)

-- pan2 ; ctl=pan[:-0.1,-1,1,lin;pan]:0.1,-1,1,lin
let loc = sinOsc KR 0.5 0 `in_range` (control KR "pan[" 0,control KR "pan]" 0)
in pan2 (pinkNoise 'α' AR) loc 0.05
