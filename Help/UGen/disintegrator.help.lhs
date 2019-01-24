    Sound.SC3.UGen.Help.viewSC3Help "Disintegrator"
    Sound.SC3.UGen.DB.ugenSummary "Disintegrator"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> gr_01 =
>     let x = mouseX KR 0 1 Linear 0.2
>         y = mouseY KR 0 1 Linear 0.2
>         s = sinOsc AR (mce2 400 404) 0 * 0.2
>     in X.disintegrator 'α' s x y
