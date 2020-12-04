-- freeSelfWhenDone ; mouseX is trigger
let x = mouseX KR (-1) 1 Linear 0.1
    e = linen x 1 0.1 1 DoNothing
in mrg [sinOsc AR 440 0 * e,freeSelfWhenDone e]

-- freeSelfWhenDone ; mouseX is trigger ; using RemoveSynth doneAction
let x = mouseX KR (-1) 1 Linear 0.1
    e = linen x 1 0.1 1 RemoveSynth
in sinOsc AR 440 0 * e
