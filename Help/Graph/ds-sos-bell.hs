-- https://github.com/supercollider-quarks/SynthDefPool/blob/master/pool/sos_bell.scd (ds)
let f c (g,_,_,z,o,_,_,p,_,_) =
      let mce_mean x = sum (mceChannels x) / fromIntegral (mceDegree_err x)
          freq = midiCPS (p - 12)
          amp = z * g * 16
          pan = o * 2 - 1
          -- Stretched harmonic series
          s1 = sinOsc AR (mce [2,3,4.1,5.43,6.8,8.21] * freq) 0 * mce [1,0.9,0.8,0.7,0.6,0.5] * 0.1
          s2 = let e_dat = Envelope [0,1,0.3,0.2,0] [0,0.3,0.3,0.3] [] Nothing Nothing 0
               in s1 * envGen AR g 1 0 1 DoNothing e_dat
          -- A bit of FM adds 'warble'
          s3 = s2 * (lfTri AR (X.rRandN 6 (c,'α') 1.0 1.8) 1 * 0.3 + 0.7)
          -- Mix down the partials in the main sound
          s4 = mce_mean s3
          strike = let e_dat = Envelope [0,1,0.2,0.1,0] [0,0.01,0,0.04] [] Nothing Nothing 0
                       e =  envGen AR g 1 0 1 DoNothing e_dat
                   in sinOsc AR (lfNoise1 (c,'β') AR (freq * 36) * 100 + (freq * 8 )) 1 * 0.1 * e
          hum = let e_dat = Envelope [0,0.05,0.05,0] [0.5,0.5,1] [] Nothing Nothing 0
                    e = envGen AR g 1 0 1 DoNothing e_dat
                in mce_mean (sinOsc AR (mce2 (freq * 1.01) (freq * 0.47)) 0 * e)
      in pan2 (s4 + strike + hum) pan amp
in mix (eventVoicer 16 f) * control KR "gain" 1
