-- vOsc3 ; mouse selects buffer and set amplitude ; see vOsc help for setup
let n = 8
    b = control KR "tbl" 0
    x = mouseX KR b (b + n - 1) Linear 0.1
    y = mouseY KR 0.01 0.2 Exponential 0.2
    o1 = vOsc3 AR x 120 121 129
    o2 = vOsc3 AR x 119 123 127
in mce2 o1 o2 * y
