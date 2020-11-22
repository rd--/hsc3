-- MiOmi ; basic
X.miOmi AR 0 0 {-pit-} 50 0.2 0.25 {-level1-} 1 {-level2-} 0.5 {-ratio1-} 0.49 0.5 {-fm1-} 0.5 0 0 0 0 0.5 0 0.5 0.5 0.2 0.5 * 0.25

-- MiOmi ; external FM
let fmfreq = mouseX KR 2 1000 Linear 0.2
    fmamp = mouseY KR 0 1 Linear 0.2
    extFM = sinOsc AR fmfreq 0 * fmamp
in X.miOmi AR extFM 0 {-pit-} 40 0.2 0.25 0.5 0.5 0.5 0.5 {-fm1-} 0.5 {-fm2-} 0.5 0 {-xfb-} 0.1 0 {-cutoff-} 0.9 0 0.5 0.5 0.2 0.5 * 0.5

-- MiOmi ; filter and cutoff modulation
let filtmod = range 0 1 (lfTri KR 0.23 0)
    gat = lfPulse KR 6 0 0.2
    cf = range 0 0.5 (lfTri KR 0.2 0)
in X.miOmi AR 0 gat 48 {-contour-} 0.1 0.25 0.5 0.5 0.5 0.5 {-fm1-} 0.5 {-fm2-} 0.5 {-fb-} 0.5 0 filtmod cf 0 {-strength-} 0 0.5 0.2 0.5

-- MiOmi ; cross feedback
let xfb = range 0 1 (lfTri KR 0.2 0)
in X.miOmi AR 0 0 {-pit-} 43 0.2 0.25 0.5 0.5 0.5 0.5 0 0 0 {-xfb-} xfb 0 0.5 0 0.5 0.5 0.2 0.5 * 0.25
