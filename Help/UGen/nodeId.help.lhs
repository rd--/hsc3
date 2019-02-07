    Sound.SC3.UGen.Help.viewSC3Help "NodeId"
    Sound.SC3.UGen.DB.ugenSummary "NodeId"

> import Sound.SC3 {- hsc3 -}

NodeId, except that negative nodeId are not reported.

> g_01 = sinOsc AR (mce2 (-100) (nodeID IR * 100)) 0 * 0.1

    audition_at (-1,AddToHead,1,[]) g_01
    audition_at (8,AddToHead,1,[]) g_01
