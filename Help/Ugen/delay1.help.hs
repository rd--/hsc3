-- delay1
let s = impulse ar 1 0 * 0.25 in s + (delay1 s)

-- delay1 ; left=original, right=subtract delayed from original
let z = dustId 'α' ar 1000 * 0.1 in mce2 z (z - delay1 z)
