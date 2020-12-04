-- random sine waves (jmcc) #1 ; texture=overlap,2,5,12,inf
let f = rand 'α' 0 2000
    o = fSinOsc AR f 0 * 0.02
in pan2 o (rand 'β' (-1) 1) 1
