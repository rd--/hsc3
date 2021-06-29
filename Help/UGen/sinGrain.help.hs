-- sinGrain
X.sinGrain ar (impulse kr 10 0) 0.1 (range 440 880 (whiteNoise 'α' kr)) * 0.1

-- sinGrain ; mouse control
let x = mouseX kr 0.001 0.2 Linear 0.1
    y = mouseX kr 90 600 Linear 0.1
in X.sinGrain ar (dust 'β' kr 25) x y * 0.1

-- sinGrain ; ln 2021-04-13 https://lukasnowok.github.io/spectrology/
X.sinGrain ar (impulse ar (sinOsc ar (1/6) 0 `in_range` (8,4000)) 0) 0.005 10000

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
