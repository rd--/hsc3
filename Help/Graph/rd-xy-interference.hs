-- xy-interference (rd, 2006-10-28)
let x = mouseX KR 20 22000 Linear (mce2 0.005 0.025)
    y = mouseY KR 20 22000 Linear (mce2 0.005 0.075)
    nd = let n = lfNoise0 'α' KR (mce2 5 9)
             a = sinOsc AR (x + n) 0
             b = sinOsc AR y 0
         in a * b
in mix (Protect.uclone_all 'β' 3 nd)
