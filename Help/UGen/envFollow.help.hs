-- envFollow
let z = soundIn 0
    d = mouseX KR 0.990 0.999 Linear 0.2
    c = X.envFollow KR z d
    o = pinkNoise 'α' AR * c
in mce2 z o
