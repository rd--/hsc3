-- index ; index buffer for frequency values
let b = asLocalBuf 'α' [50,100,200,400,800,1600]
    f = index b (range 0 6 (lfSaw KR 2 0))
in sinOsc AR (mce [f,f * 9]) 0 * 0.1

-- index ; mouse control
let b = asLocalBuf 'α' [200, 300, 400, 500, 600, 800]
    f = index b (mouseX KR 0 7 Linear 0.2)
in sinOsc AR f 0 * 0.1
