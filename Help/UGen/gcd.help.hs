-- gcd
let x = mouseX KR (-200) 200 Linear 0.2
    y = mouseY KR (-200) 200 Linear 0.2
in sinOsc AR ((sinOsc KR 0.3 0 * 20) `gcdE` mce2 x y * 30 + 500) 0 * 0.1
