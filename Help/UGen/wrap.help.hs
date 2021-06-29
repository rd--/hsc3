-- wrap ; ln 2021-04-15 https://lukasnowok.github.io/spectrology/
let s = sinOsc ar (sinOsc ar (1 / 40) 0 * 100 + 20000) 0
in wrap s (xLine ar (-1.0) (-0.01) 20 DoNothing) 1 * 0.1

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
