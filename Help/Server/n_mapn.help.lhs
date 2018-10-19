    Sound.SC3.Server.Help.viewServerHelp "/n_mapn"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> f_01 nm def ix = control_f64 KR (Just ix) nm def
> g_01 = out (f_01 "bus" 0 0) (sinOsc AR (f_01 "freq" 440 1) 0 * f_01 "amp" 0.1 2)
> s_01 = synthdef "sin" g_01
> m_01 = d_recv s_01
> m_02 = s_new "sin" 1001 AddToHead 1 []
> m_03 = n_mapn 1001 [(0,0,3)]

    withSC3 (async (d_recv s_01) >> mapM_ sendMessage [m_02,m_03])

    withSC3 (sendMessage (n_mapn 1001 [(0,0,3)]))
    withSC3 (sendMessage (c_setn [(0,[1,880,0.2])]))
    withSC3 (sendMessage (c_setn [(0,[0,220,0.3])]))

    withSC3 (sendMessage (n_set1 1001 "bus" 1))
    withSC3 (sendMessage (n_set1 1001 "freq" 440))
    withSC3 (sendMessage (n_set1 1001 "amp" 0.2))
    withSC3 (sendMessage (n_setn 1001 [(0,[1,880,0.2])]))

n_mapn and n_setn only work if the control is given as an index and not as a name.

s.sendMsg("/n_mapn",1001,"bus",0,3);
s.sendMsg("/c_setn",0,3,0,880,0.1);

s.sendMsg("/n_mapn",1001,0,0,3);
s.sendMsg("/c_setn",0,3,0,880,0.1);
s.sendMsg("/c_setn",0,3,1,220,0.3);
