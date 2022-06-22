    Sound.Sc3.Lang.Help.viewServerHelp "/b_free"

> import Sound.Sc3 {- hsc3 -}

It is safe to free un-allocated buffers.

    withSc3 (async (b_free (2 ^ 15)))

There is no multiple buffer form.

    withSc3 (mapM_ (\k -> async (b_free k)) [0..256])
