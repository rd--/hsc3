-- rEvent ; c=freq ; harmonics of A2
let f c (g,_,_,_,_,_,_,_) = sinOsc AR (110 * (constant c + 1)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.05

-- rEvent ; x=freq ; 0-1 = full range ; i.e. mpe controller or large touch controller
let f _ (g,x,_,_,_,_,_,_) = sinOsc AR (midiCPS (x * 127)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; x=freq ; 0-1 = partial range ; i.e. small touch controller
let f _ (g,x,_,_,_,_,_,_) = sinOsc AR (midiCPS (x * 24 + 48)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; y=freq ; 0-1 = partial range
let f _ (g,_,y,_,_,_,_,_) = sinOsc AR (midiCPS (y * 24 + 48)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; z=freq ; 0-1 = partial range
let f _ (g,_,_,z,_,_,_,_) = sinOsc AR (midiCPS (z * 24 + 48)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; o=freq ; 0-1 = partial range
let f _ (g,_,_,_,o,_,_,_) = sinOsc AR (midiCPS (o * 24 + 48)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; rx=freq ; 0-1 = partial range
let f _ (g,_,_,_,_,rx,_,_) = sinOsc AR (midiCPS (rx * 24 + 48)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; ry=freq ; 0-1 = partial range
let f _ (g,_,_,_,_,_,ry,_) = sinOsc AR (midiCPS (ry * 24 + 48)) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; p=freq
let f _ (g,_,_,_,_,_,_,p) = sinOsc AR (midiCPS p) 0 * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.1

-- rEvent ; x=freq y=amp ; 0-1 = partial range
let f _ (g,x,y,_,_,_,_,_) = sinOsc AR (midiCPS (x * 12 + 48)) 0 * y * lagUD g 0.05 1
in mix (rEventVoicer 16 f) * control KR "gain" 0.2

-- rEvent ; x=freq z=amp ; 0-1 = partial range ; 24ET
let f _ (g,x,_,z,_,_,_,_) = sinOsc AR (midiCPS ((x * 12 + 48) `roundTo` 0.5)) 0 * z * g
in mix (rEventVoicer 16 f) * control KR "gain" 0.5

-- rEvent ; x=freq y=pan z=amp ; 0-1 = partial range ; 24ET
let f _ (g,x,y,z,_,_,_,_) =
      pan2 (sinOsc AR (midiCPS ((x * 12 + 48) `roundTo` 0.5)) 0) (y * 2 - 1) (z * g)
in mix (rEventVoicer 16 f) * control KR "gain" 0.5

-- rEvent ; p=freq y=pan z=amp ; 0-1 = partial range ; 24ET
let f _ (g,_,y,z,_,_,_,p) =
      pan2 (sinOsc AR (midiCPS p) 0) (y * 2 - 1) (z * g)
in mix (rEventVoicer 16 f) * control KR "gain" 0.5

---- ; reference tones
mix (sinOsc AR (midiCPS (mce [48,60,72])) 0 * mce [0.1,0.01,0.1])
