-- event ; c=freq ; harmonics of A2
let f (c,g,_,_,_,_,_,_,_,_,_) = sinOsc ar (110 * (constant c + 1)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.05

-- event ; x=freq ; 0-1 = full range ; i.e. mpe controller or large touch controller
let f (_,g,x,_,_,_,_,_,_,_,_) = sinOsc ar (midiCps (x * 127)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; x=freq ; 0-1 = partial range ; i.e. small touch controller
let f (_,g,x,_,_,_,_,_,_,_,_) = sinOsc ar (midiCps (x * 24 + 48)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; y=freq ; 0-1 = partial range
let f (_,g,_,y,_,_,_,_,_,_,_) = sinOsc ar (midiCps (y * 24 + 48)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; z=freq ; 0-1 = partial range
let f (_,g,_,_,z,_,_,_,_,_,_) = sinOsc ar (midiCps (z * 24 + 48)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; o=freq ; 0-1 = partial range
let f (_,g,_,_,_,o,_,_,_,_,_) = sinOsc ar (midiCps (o * 24 + 48)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; rx=freq ; 0-1 = partial range
let f (_,g,_,_,_,_,rx,_,_,_,_) = sinOsc ar (midiCps (rx * 24 + 48)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; ry=freq ; 0-1 = partial range
let f (_,g,_,_,_,_,_,ry,_,_,_) = sinOsc ar (midiCps (ry * 24 + 48)) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; p=freq ; p is a pitch value in (0,1) calculated by the control input system
let f (_,g,_,_,_,_,_,_,p,_,_) = sinOsc ar (unitCps p) 0 * g
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; p+px=freq
let f (_,w,_,_,_,_,_,_,p,px,_) = sinOsc ar (midiCps (p * 127 + px)) 0 * w
in mix (voicer 16 f) * control kr "gain" 0.1

-- event ; p+px=freq ; z*z=ampl ; w=gate
let f (_,w,_,_,z,_,_,_,p,px,_) = sinOsc ar (midiCps (p * 127 + px)) 0 * w * z * z * 4
in mix (voicer 16 f) * control kr "gain" 1

-- event ; x=freq y=amp ; 0-1 = partial range
let f (_,g,x,y,_,_,_,_,_,_,_) = sinOsc ar (midiCps (x * 12 + 48)) 0 * y * lagUD g 0.05 1
in mix (voicer 16 f) * control kr "gain" 0.2

-- event ; x=freq z=amp ; 0-1 = partial range ; 24ET
let f (_,g,x,_,z,_,_,_,_,_,_) = sinOsc ar (midiCps ((x * 12 + 48) `roundTo` 0.5)) 0 * z * g
in mix (voicer 16 f) * control kr "gain" 0.5

-- event ; p=freq z=amp
let f (_,g,_,_,z,_,_,_,p,_,_) = sinOsc ar (unitCps p * 2) 0 * g * z
in mix (voicer 16 f) * control kr "gain" 1.0

-- event ; x=freq y=pan z=amp ; 0-1 = partial range ; 24ET
let f (_,g,x,y,z,_,_,_,_,_,_) =
      pan2 (sinOsc ar (midiCps ((x * 12 + 48) `roundTo` 0.5)) 0) (y * 2 - 1) (z * g)
in mix (voicer 16 f) * control kr "gain" 0.5

-- event ; p=freq y=pan z=amp ; 0-1 = partial range ; 24ET
let f (_,g,_,y,z,_,_,_,p,_,_) =
      pan2 (sinOsc ar (unitCps p `roundTo` 0.5) 0) (y * 2 - 1) (z * g)
in mix (voicer 16 f) * control kr "gain" 0.5

-- event ; simple frequency modulation
let f (_,g,x,y,z,o,_,_,_,_,_) =
      let cR = 1 -- carrier ratio
          mR = 2 -- modulation ratio
          mI = y * 3 -- modulation index
          f0 = midiCps (x * 25 + 48)
          f1 = f0 * cR
          f2 = f0 * mR
          o2 = sinOsc ar f2 0
          o1 = sinOsc ar (f1 + (o2 * mI * f2)) 0
      in pan2 o1 (o * 2 - 1) (z * z * g)
in mix (voicer 16 f) * control kr "gain" 4

---- ; reference tones
mix (sinOsc ar (midiCps (mce [48,60,72])) 0 * mce [0.1,0.01,0.1])
