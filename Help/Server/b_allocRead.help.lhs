    Sound.SC3.Lang.Help.viewServerHelp "/b_allocRead"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Read a large audio file into a buffer.

> f_00 = "/home/rohan/data/audio/xenakis/jonchaies.wav"
> m_00 = b_allocRead 0 f_00 0 0

    withSC3 (async m_00)

Audio data is loaded in IEEE 32-bit form, so in-memory storage can be
greater than on-disk storage.

$ sndfile-info data/audio/xenakis/jonchaies.wav
Sample Rate : 44100
Frames      : 42271320
Duration    : 00:15:58.533
$ du -h data/audio/xenakis/jonchaies.wav
162M    data/audio/xenakis/jonchaies.wav
$

    round ((16 * 60 * 44100 * 4 * 2) / (1024 * 1024)) == 323
    round ((42271320 * 2 * 4) / (1024 * 1024)) == 323

Query buffer.

> q_00 :: Connection UDP ()
> q_00 = do
>   sendMessage (b_query [0])
>   r <- waitReply "/b_info"
>   liftIO (print r)

    withSC3 q_00

Play buffer.

> g_00 =
>   let bufnum = 257
>       s = bufRateScale KR bufnum
>   in playBuf 1 AR bufnum s 1 0 NoLoop RemoveSynth

Re-read file into buffer with the same identifier.  The existing
buffer is freed but not before further memory is allocated,
intermediate in-memory use is greater than final memory use.

    withSC3 (async (b_allocRead 0 fn 0 0))

Free buffer.  Memory is immediately made free.

    withSC3 (async (b_free 0))

Small sample buffer

> f_01 = "/home/rohan/data/audio/metal.wav"

    withSC3 (async m_01)

> m_01 :: Message
> m_01 = b_allocRead 0 f_01 0 0
