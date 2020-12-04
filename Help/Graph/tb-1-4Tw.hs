-- http://sccode.org/1-4Tw (tb)
let im = mix (impulse AR (mce3 1 (1/3) (1/5)) (mce3 0 0.133 0.5))
    f i = tDuty AR (max 0.25 (timer i)) 0 DoNothing 1 0
    g n i = sinOsc AR ((4000 + (i * 500))) 0 * decay2 n 0.01 0.2
    nd = zipWith g (iterate f im) [0..9]
in splay (mce nd) 1 1 0 True
