-- wrapIndex ; c.f. index
let b = asLocalBufId 'α' [200,300,400,500,600,800]
    x = mouseX kr 0 18 Linear 0.1
    f = wrapIndex b x
in sinOsc ar f 0 * 0.1
