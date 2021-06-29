-- rm-octaver (miller puckette) ; http://crca.ucsd.edu/~msp/techniques/latest/book-html/node77.html
let defaultPitch x = pitch x 440 60 4000 100 16 1 0.01 0.5 1 0
    rm_octaver i =
      let p = defaultPitch i
          [f,tr] = mceChannels p
      in lag3 tr 0.1 * sinOsc ar (f * 0.5) 0 * i + i
in rm_octaver (soundIn (control kr "in" 0))
