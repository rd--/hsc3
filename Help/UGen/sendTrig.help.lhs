    Sound.SC3.UGen.Help.viewSC3Help "SendTrig"
    Sound.SC3.UGen.DB.ugenSummary "SendTrig"

> import Control.Monad {- base -}
> import Data.Maybe {- base -}

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Retrieve a single message

> g_01 =
>     let s = lfNoise0 'α' KR 5
>         o = sinOsc AR (s * 200 + 500) 0 * 0.1
>     in mrg [o,sendTrig s 0 s]

> f_01 :: Transport m => m Message
> f_01 = withNotifications (waitReply "/tr")

    withSC3 f_01

Send random triggers, request notifications, then for each trigger start a synth (s_03).

> g_02 =
>     let t = dust 'α' KR 1.0
>     in sendTrig t 0 t

> g_03 =
>     let freq = control KR "freq" 440
>         env = envGen KR 1 0.1 0 1 RemoveSynth (envPerc 0.01 1)
>     in sinOsc AR freq 0 * env

> s_03 = synthdef "s_03" (out 0 g_03)

> f_02 :: Transport m => m ()
> f_02 = do
>   r <- waitReply "/tr"
>   let (_,_,n) = unpack_tr_err r
>   sendMessage (s_new "s_03" (-1) AddToTail 1 [("freq",110 + (n * 110))])

> f_03 :: Transport m => m ()
> f_03 = do
>   play g_02
>   async_ (notify True)
>   async_ (d_recv s_03)
>   repeatM_ f_02

     withSC3 f_03
