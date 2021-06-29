-- indexL ; index buffer for frequency values
let b = asLocalBuf 'α' [50,100,200,400,800,1600]
    ph = 1 -- 0
    i = range 0 7 (lfSaw kr 0.1 ph)
in sinOsc ar (mce2 (indexL b i) (index b i)) 0 * 0.1

-- indexL ; mouse control
let b = asLocalBuf 'α' [200,300,400,500,600,800]
    x = mouseX kr 0 7 Linear 0.2
in sinOsc ar (mce2 (indexL b x) (index b x)) 0 * 0.1
