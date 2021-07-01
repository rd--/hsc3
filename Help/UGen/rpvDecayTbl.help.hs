-- RPVDecayTbl ; random decay multipliers
let b0 = clearBuf (localBufId 'α' 1 2048)
    b1 = asLocalBufId 'β' (mceChannels (X.rRandNId 1024 'γ' 0.5 0.975))
    b2 = clearBuf (localBufId 'δ' 1 1024)
in ifft' (X.rpvDecayTbl (fft' b0 (soundIn 0)) b1 b2)

