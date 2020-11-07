-- standard2DL ; mouse-controlled param
let x0 = 4.9789799812499
    y0 = 5.7473416156381
in standard2DL AR 11025 44100 (mouseX KR 0.9 4 Linear 0.2) x0 y0 * 0.3

-- standard2DL ; as a frequency control
let x0 = 4.9789799812499
    y0 = 5.7473416156381
    k = mouseX KR 0.9 4 Linear 0.2
    f = standard2DL AR 10 20 k x0 y0 * 800 + 900
in sinOsc AR f 0 * 0.3
