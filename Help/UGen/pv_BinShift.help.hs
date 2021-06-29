-- pv_BinShift ; default values
ifft' (pv_BinShift (ffta 'α' 2048 (soundIn 0) 0.5 0 1 0) 1 0 0) * 0.25

-- pv_BinShift ; mouse control
let z = soundIn 0
    x = mouseX kr (-10) 100 Linear 0.1 -- shift
    y = mouseY kr 1 4 Linear 0.1 -- stretch
    b = mouseButton kr 0 1 0.2
    pv = pv_BinShift (ffta 'β' 2048 z 0.5 0 1 0) y x b
in ifft' pv * 0.25
