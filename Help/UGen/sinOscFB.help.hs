-- sinOscFB
let x = mouseX KR 0 4 Linear 0.2
in sinOscFB AR (mce2 400 301) x * 0.1

-- sinOscFB
let y = mouseY KR 10 1000 Exponential 0.2
    x = mouseX KR (pi/2) pi Linear 0.2
in sinOscFB AR y x * 0.1

-- sinOscFB
let y = mouseY KR 1 1000 Exponential 0.2
    x = mouseX KR (pi/2) pi Linear 0.2
in sinOscFB AR (100 * sinOscFB AR y 0 + 200) x * 0.1
