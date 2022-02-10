-- RPVDecayTbl ; random decay multipliers
let b0 = clearBuf (localBuf 1 2048)
    b1 = asLocalBuf (mceChannels (X.randN 1024 0.5 0.975))
    b2 = clearBuf (localBuf 1 1024)
in ifft' (X.rpvDecayTbl (fft' b0 (soundIn 0)) b1 b2)

-- RPVDecayTbl ; random decay multipliers ; id
let b0 = clearBuf (localBufId 'α' 1 2048)
    b1 = asLocalBufId 'β' (mceChannels (X.randNId 1024 'γ' 0.5 0.975))
    b2 = clearBuf (localBufId 'δ' 1 1024)
in ifft' (X.rpvDecayTbl (fft' b0 (soundIn 0)) b1 b2)
