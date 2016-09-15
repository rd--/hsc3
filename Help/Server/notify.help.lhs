> Sound.SC3.Server.Help.viewServerHelp "/notify"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> let g = synthdef "g" (out 0 (sinOsc AR (rand 'α' 440 880) 0 * 0.1))
> in withSC3 (async (d_recv g))

> withSC3 (withNotifications (do {send (s_new "g" (-2) AddToHead 1 [])
>                                ;waitReply "/n_go"}))
