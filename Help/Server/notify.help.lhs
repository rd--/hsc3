/notify            Register to receive notifications from server

int - one to receive notifications, zero to stop receiving them.

If argument is one, server will remember your return address and
send you notifications. if argument is zero, server will stop
sending you notifications.

Asynchronous. Replies to sender with /done when complete.

> import Sound.OpenSoundControl
> import Sound.SC3.ID

> let g = synthdef "g" (out 0 (sinOsc AR (rand 'a' 440 880) 0 * 0.1))

> let s_alloc n a t c = do

> withSC3 (\fd -> do {_ <- async fd (d_recv g)
>                    ;_ <- async fd (notify True)
>                    ;send fd (s_new "g" (-2) AddToHead 1 [])
>                    ;m <- waitAddressMessage fd "/n_go"
>                    ;_ <- async fd (notify False)
>                    ;return m})
