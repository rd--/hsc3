    Sound.SC3.Server.Help.viewServerHelp "/c_getn"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Get control bus data.

> get_c :: Transport m => m ()
> get_c = do
>   sendMessage (c_getn [(0,3)])
>   r <- waitReply "/c_setn"
>   liftIO (print r)

    withSC3 (sendMessage (c_setn [(0,[1,880,0.5])]))
    withSC3 get_c

Function to get and unpack control bus data.

    withSC3 (c_getn1_data (0,3))
