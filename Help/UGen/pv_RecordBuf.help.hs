-- pv_RecordBuf ; analyse signal and store to buffer
let snd_buf = control kr "buf" 0
    rec_buf = control kr "rec" 10
    fft_buf = localBufId 'α' 1 1024
    i = playBuf 1 ar snd_buf (bufRateScale kr snd_buf) 1 0 NoLoop RemoveSynth
    c0 = fft fft_buf i 0.25 1 1 0
    c1 = X.pv_RecordBuf c0 rec_buf 0 1 0 0.25 1
in mrg2 i c1

---- ; load soundfile
fn = sfRequire "a11wlk01.wav"
withSC3 (async (b_allocRead 0 fn 0 0))

---- ; allocate analysis buffer ; duration=5, frame-size=1024, hop-size=0.25, sample-rate=48000
withSC3 (async (b_alloc 10 (pv_calcPVRecSize 5 1024 0.25 48000) 1))
