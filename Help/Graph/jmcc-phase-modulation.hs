-- phase modulation with slow beats (jmcc) #6 texture=overlap,4,4,4,inf
let nrec n f st = if n > 0 then nrec (n - 1) f (f st) else st
    x = mouseX KR 100 6000 Exponential 0.2 {- random freq of new events -}
    y = mouseY KR 0 2 Linear 0.2 {- modulation index -}
    o (e,a) = let f = rand e 0 x
              in (succ e,fSinOsc AR (mce [f,f + rand2 'α' 1]) 0 * y + a)
    ph = snd (nrec (3::Int) o ('β',0))
    freq = rand 'γ' 0 x
in sinOsc AR (mce [freq, freq + rand2 'δ' 1]) ph * 0.1
