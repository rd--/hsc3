> Sound.SC3.Server.Help.viewServerHelp "/b_free"

It is safe to free un-allocated buffers.

> withSC3 (async (b_free (2 ^ 15)))

There is no multiple buffer form.

> withSC3 (mapM_ (\k -> async (b_free k)) [0..256])
