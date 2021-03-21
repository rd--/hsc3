-- sonLPC
let nf = 1024 * 2
    hop = 1 / 4
    poles = mouseY KR 4 80 Linear 0.2 -- MAXPOLES=80
    sig = soundIn 0
    chain = X.sonLPC AR (localBuf 'α' 1 nf) sig hop poles
    freq = mouseX KR 50 200 Linear 0.2
    exci = saw AR freq
in pan2 (X.sonLPCSynthIn AR chain exci) 0 1

-- sonLPC ; event control
let f c (g,x,y,z,o,_,_,_,_,_) =
      let nf = 1024 * 2
          hop = 1 / 4
          poles = linLin y 0 1 5 80
          sig = soundIn 0 * 4
          chain = X.sonLPC AR (localBuf c 1 nf) sig hop poles
          freq = midiCPS (x * 24 + 36)
          exci = saw AR freq * z * g
      in pan2 (X.sonLPCSynthIn AR chain exci) (o * 2 - 1) 1
in mix (rEventVoicer 4 f) * control KR "gain" 1