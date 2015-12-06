    Sound.SC3.UGen.Help.viewSC3Help "Disintegrator"
    Sound.SC3.UGen.DB.ugenSummary "Disintegrator"

> import Sound.SC3 {- hsc3 -}

> gr_01 =
>     let x = mouseX KR 0 1 Linear 0.2
>         y = mouseY KR 0 1 Linear 0.2
>         s = sinOsc AR (mce2 400 404) 0 * 0.2
>     in disintegrator 'α' s x y
