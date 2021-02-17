-- tremulate (jmcc) #1 ; texture=xfade,0.5,2,inf
let f = rand 'α' 500 900
    o = fSinOsc AR (f * mce [1,1.2,1.5,1.8]) 0
    r = X.randN 4 'β' 30 90
    a = max 0 (lfNoise2 'γ' KR r) * 0.1
    l = X.randN 4 'δ' (-1) 1
in mix (pan2 o l a)

-- tremulate (jmcc) #1 ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let s = fSinOsc AR ((x * 400 + 500) * mce [1,1.2,1.5,1.8]) 0
          r = X.randN 4 'β' 30 90
          a = max 0 (lfNoise2 'γ' KR (r * (0.75 + rx))) * z
          l = X.randN 4 'δ' (-1) 1
      in mix (pan2 s (l + (o * 2 - 1)) (a * lagUD g 0 (ry * 2)))
in combN (mix (rEventVoicer 16 f) * control KR "gain" 0.5) 0.1 0.1 1
