-- coyote
let i = soundIn 0
    c = X.coyote KR i 0.2 0.2 0.01 0.5 0.05 0.1
    o = pinkNoise 'α' AR * decay c 1 * 0.25
in mce2 (i * 0.25) o
