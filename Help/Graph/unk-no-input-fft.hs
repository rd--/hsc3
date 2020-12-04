-- no input fft ; https://www.listarc.bham.ac.uk/liasts/sc-users/msg63383.html
let e_01 :: Envelope Double
    e_01 = envelope [0.0001,1,0.0001,0.01,0.0001] (map (* 512) (normalizeSum [1,2,2,50])) [EnvExp]
    e_ix e n = constant (envelope_at e (fromIntegral n))
    fft_size = 1024
    fft_f z = fft (localBuf z 1 (constant fft_size)) (dc AR 0) 0.5 0 1 0
    bin_f z _mag phase bin =
      (e_ix e_01 bin * range 10 50 (sinOsc KR (rand (z,bin) 0 0.05) (rand (z,bin) 0 (2 * pi)))
      ,phase)
    gen_f z = pvcollect (fft_f z) fft_size (bin_f z) 0 511 0
in ifft (mce2 (gen_f 'α') (gen_f 'β')) 1 0
