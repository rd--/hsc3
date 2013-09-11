> Sound.SC3.Server.Help.viewServerHelp "/b_allocRead"

Read a large audio file into a buffer.

> let fn = "/home/rohan/data/audio/xenakis/jonchaies.wav"
> in withSC3 (async (b_allocRead 0 fn 0 0))

Audio data is loaded in IEEE 32-bit form, so in-memory storage can be
greater than on-disk storage.

$ sndfile-info data/audio/xenakis/jonchaies.wav
Sample Rate : 44100
Frames      : 42271320
Duration    : 00:15:58.533
$ du -h data/audio/xenakis/jonchaies.wav
162M    data/audio/xenakis/jonchaies.wav
$

> round ((16 * 60 * 44100 * 4 * 2) / (1024 * 1024)) == 323
> round ((42271320 * 2 * 4) / (1024 * 1024)) == 323

Query buffer.

> withSC3 (do {send (b_query [0])
>             ;r <- waitReply "/b_info"
>             ;liftIO (print r)})

Play buffer.

> let s = bufRateScale KR 0
> in audition (out 0 (playBuf 1 AR 0 s 1 0 NoLoop RemoveSynth))

Re-read file into buffer with the same identifier.  The existing
buffer is freed but not before further memory is allocated,
intermediate in-memory use is greater than final memory use.

> let fn = "/home/rohan/data/audio/xenakis/jonchaies.wav"
> in withSC3 (async (b_allocRead 0 fn 0 0))

Free buffer.  Memory is immediately made free.

> withSC3 (async (b_free 0))
