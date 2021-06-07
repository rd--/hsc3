-- gVerb ; mono reverb
let i = impulse AR (mce2 1 2) 0
    c = lfCub AR (mce2 900 1200) 0
    s = decay i (mce2 0.05 0.25) * c * 0.05
in mix (gVerb s 10 3 0.5 0.5 15 1 0.7 0.5 300)
