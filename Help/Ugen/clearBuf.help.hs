-- clearBuf
let buf = clearBuf (localBuf 1 2048)
    x = mouseX kr 1 2 Linear 0.2
    r = playBuf 1 ar buf x 1 0 Loop DoNothing * 0.1
    wr p i = bufWr buf (p `in_range` (0,bufFrames kr buf)) Loop i
in mrg2 r (wr (lfNoise0 ar 530) (whiteNoise ar))

-- clearBuf ; id
let buf = clearBuf (localBufId 'α' 1 2048)
    x = mouseX kr 1 2 Linear 0.2
    r = playBuf 1 ar buf x 1 0 Loop DoNothing * 0.1
    wr p i = bufWr buf (p `in_range` (0,bufFrames kr buf)) Loop i
in mrg2 r (wr (lfNoise0Id 'β' ar 530) (whiteNoiseId 'γ' ar))

