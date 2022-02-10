-- deep trip (jmcc) #9 ; texture=overlap,4,12,4,inf
let f = midiCps (lfNoise1 kr (rand 0 0.3) * 60 + 70)
    a'' = sinOsc kr (rand 0 40) 0 * 0.1
    a' = max 0 (lfNoise1 kr (rand 0 8) * a'')
    a = lfNoise2 ar (f * rand 0 0.5) * a'
    z = sinOsc ar f 0 * a
    s = pan2 z (lfNoise1 kr (rand 0 5)) 1
    c0 = combN s 0.5 (X.randN 2 0.3 0.5) 20
    c1 = combN s 0.5 (X.randN 2 0.3 0.5) 20
in s + c0 + c1

-- deep trip (jmcc) #9 ; texture=overlap,4,12,4,inf ; id
let f = midiCps (lfNoise1Id 'α' kr (randId 'β' 0 0.3) * 60 + 70)
    a'' = sinOsc kr (randId 'γ' 0 40) 0 * 0.1
    a' = max 0 (lfNoise1Id 'δ' kr (randId 'ε' 0 8) * a'')
    a = lfNoise2Id 'ζ' ar (f * randId 'η' 0 0.5) * a'
    z = sinOsc ar f 0 * a
    s = pan2 z (lfNoise1Id 'θ' kr (randId 'ι' 0 5)) 1
    c0 = combN s 0.5 (X.randNId 2 'κ' 0.3 0.5) 20
    c1 = combN s 0.5 (X.randNId 2 'λ' 0.3 0.5) 20
in s + c0 + c1
