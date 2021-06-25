-- clearBuf
let buf = clearBuf (localBuf 'α' 1 2048)
    x = mouseX KR 1 2 Linear 0.2
    r = playBuf 1 AR buf x 1 0 Loop DoNothing * 0.1
    wr p i = bufWr buf (p `in_range` (0,bufFrames KR buf)) Loop i
in mrg2 r (wr (lfNoise0 'β' AR 530) (whiteNoise 'γ' AR))

