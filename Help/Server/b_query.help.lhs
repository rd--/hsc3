> Sound.SC3.Server.Help.viewServerHelp "/b_query"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Allocate and generate wavetable buffer
> withSC3 (do {_ <- async (b_alloc 0 256 1)
>             ;let f = [Normalise,Wavetable,Clear]
>              in send (b_gen_sine1 0 f [1,1/2,1/3,1/4,1/5])})

Query buffer
> withSC3 (do {send (b_query [0])
>             ;r <- waitReply "/b_info"
>             ;liftIO (print r)})

Play buffer
> audition (out 0 (osc AR 0 220 0 * 0.1))

Free buffer
> withSC3 (async (b_free 0))

Query multiple un-allocated buffers
> withSC3 (do {send (b_query [2^14,2^15])
>             ;r <- waitReply "/b_info"
>             ;liftIO (print r)})
