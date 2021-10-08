    Sound.SC3.Lang.Help.viewServerHelp "/b_query"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Allocate and generate wavetable buffer

> mk_b :: Transport m => m ()
> mk_b = do
>   _ <- async (b_alloc 0 256 1)
>   let f = [Normalise,Wavetable,Clear]
>   sendMessage (b_gen_sine1 0 f [1,1/2,1/3,1/4,1/5])

    withSC3 mk_b

Query buffer

> qr_b :: Transport m => m ()
> qr_b = do
>   sendMessage (b_query [0])
>   r <- waitReply "/b_info"
>   liftIO (print r)

    withSC3 qr_b

Variant that unpacks the result.

Query is of (buffer-id/int,#-frames/int,#-channels/int,sample-rate/float).

    withSC3 (b_query1_unpack 0)

Play buffer

> g_01 = osc AR 0 220 0 * 0.1

Free buffer

    withSC3 (async (b_free 0))

Query multiple un-allocated buffers

> qr_unalloc :: Transport m => m ()
> qr_unalloc = do
>   sendMessage (b_query [2^14,2^15])
>   r <- waitReply "/b_info"
>   liftIO (print r)

    withSC3 qr_unalloc
