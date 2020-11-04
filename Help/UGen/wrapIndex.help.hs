-- wrapIndex ; c.f. index
let b = asLocalBuf 'α' [200,300,400,500,600,800]
    x = mouseX KR 0 18 Linear 0.1
    f = wrapIndex b x
in sinOsc AR f 0 * 0.1
