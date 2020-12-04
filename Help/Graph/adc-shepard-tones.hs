-- shepard tones, alberto de campo (adc)
let interp n l r = let i = (r - l) / n in [l,l + i .. r - i]
    hanning_window n =
      let lp = pi * (-0.5)
          rp = lp + 2 * pi
          hf i = sin i * 0.5 + 0.5
      in map hf (interp n lp rp)
    freq_tbl = let amp_f i = (0.5 ** i) * 20000 in map amp_f (interp 1024 0 10)
    amp_tbl = let square x = x * x in map square (hanning_window 1024)
    b0 = asLocalBuf 'α' freq_tbl
    b1 = asLocalBuf 'β' amp_tbl
    rate = 0.1
    ratescale = 1024 / 48000 / 10 -- sample-rate
    ph = phasor AR 0 (rate * ratescale) 0 1024 0
    phases = mce (map (\n -> n * 0.1 * 1024 + ph) [0..9])
    freqs = bufRdC 1 AR b0 phases Loop
    amps = bufRdC 1 AR b1 phases Loop
in mix (sinOsc AR freqs 0 * amps) * 0.05
