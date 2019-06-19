    Sound.SC3.Server.Help.viewServerHelp "/b_getn"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.Plot {- hsc3-plot -}

Allocate and generate wavetable buffer (256 frames)

> x_00 :: DuplexOSC m => m ()
> x_00 = do
>   async_ (b_alloc 0 256 1)
>   let f = [Normalise,Clear]
>   sendMessage (b_gen_sine1 0 f [1,1/2,1/3,1/4,1/5])

    > withSC3 x_00

Run simple read...

    > d0 <- withSC3 (b_getn1_data 0 (0,255))

and draw buffer

    > plotTable [d0]

Load sound file

> fn_00 = "/tmp/dx7.wav"

> m_00 = b_allocRead 1 fn_00 0 0

    > withSC3 (async_ m_00)

Run segmented read (n-frames of data in m-frame segments)...

> x_01 :: DuplexOSC m => m [Double]
> x_01 = do
>   let m = 155
>       n = m * 3
>   b_getn1_data_segment m 1 (0,n)

    > d1 <- withSC3 x_01

and draw buffer

    > plotImpulses [d1]
