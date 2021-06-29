-- bit reduction (adc)
let downSample =
      let f = lfNoise2 kr 8
          nh = lfNoise2 kr 3
          src = blip ar (f * 200 + 300) (nh * 10 + 20)
          sr = mouseX kr 1000 (sampleRate * 0.1) Exponential 0.2
      in latch src (impulse ar sr 0) * 0.1
    bitSz = mouseY kr 1 24 Exponential 0.2
    bitRedux = roundTo downSample (0.5 ** bitSz)
in mce2 downSample bitRedux

-- bit reduction (adc) ; id
let downSample =
      let f = lfNoise2Id 'α' kr 8
          nh = lfNoise2Id 'β' kr 3
          src = blip ar (f * 200 + 300) (nh * 10 + 20)
          sr = mouseX kr 1000 (sampleRate * 0.1) Exponential 0.2
      in latch src (impulse ar sr 0) * 0.1
    bitSz = mouseY kr 1 24 Exponential 0.2
    bitRedux = roundTo downSample (0.5 ** bitSz)
in mce2 downSample bitRedux
