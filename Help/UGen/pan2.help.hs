-- pan2
let n = pinkNoise 'α' ar in pan2 n (fSinOsc kr 2 0) 0.1

-- pan2
let n = pinkNoise 'α' ar * 0.1
    x = mouseX kr (-1) 1 Linear 0.2
    y = mouseY kr 0 1 Linear 0.2
in pan2 n x y

-- pan2 ; ctl=freq:220,220,440,exp;amp:0.1,0,1,amp;pan:0,-1,1,lin
pan2 (sinOsc ar (control kr "freq" 220) 0) (control kr "pan" 0) (control kr "amp" 0.1)

-- pan2 ; ctl=pan[:-0.1,-1,1,lin;pan]:0.1,-1,1,lin
let loc = sinOsc kr 0.5 0 `in_range` (control kr "pan[" 0,control kr "pan]" 0)
in pan2 (pinkNoise 'α' ar) loc 0.05
