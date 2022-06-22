-- delTapWr
let b = localBufId 'α' 1 48000
    src = whiteNoiseId 'β' ar * 0.2 * decay (dustId 'γ' kr 3) 0.2
    tapPhase = delTapWr b src
    (tap1,tap2,tap3) = unmce3 (delTapRd b tapPhase (mce3 0.2 0.27 0.303) 1 * mce3 1 0.4 0.2)
in mce2 (src + tap2) (tap1 + tap3)
