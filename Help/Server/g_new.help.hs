    Sound.SC3.Lang.Help.viewServerHelp "/g_new"

The root node of the synthesiser tree is a group with ID zero.

By convention there is a group with ID one at the root group, but
this is only a convention.  We need to make the group.

> import Sound.SC3 {- hsc3 -}

> m_01 = g_new [(1,AddToTail,0)]

    > withSC3 (send m_00)
