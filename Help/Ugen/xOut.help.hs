-- xOut ; send signal to a bus, crossfading with existing contents
let p a b = sinOsc ar (mce2 a b) 0 * 0.1
    x = mouseX kr 0 1 Linear 0.1
    y = mouseY kr 0 1 Linear 0.1
in mrg [out  0   (p 220 221)
       ,xOut 0 x (p 330 331)
       ,xOut 0 y (p 440 441)
       ,out  0   (p 120 121)]
