-- detectIndex ; find indexes and map to an audible frequency range
let n = 6
    x = floorE (mouseX kr 0 n Linear 0.1)
    i = detectIndex (asLocalBufId 'α' [2,3,4,0,1,5]) x
in sinOsc ar (linExp i 0 n 200 700) 0 * 0.1
