-- delay1
let s = impulse AR 1 0 * 0.25 in s + (delay1 s)

-- delay1 ; left=original, right=subtract delayed from original
let z = dust 'α' AR 1000 * 0.1 in mce2 z (z - delay1 z)
