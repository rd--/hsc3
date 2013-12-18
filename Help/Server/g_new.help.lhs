> Sound.SC3.Server.Help.viewServerHelp "/g_new"

The root node of the synthesiser tree is a group with ID zero.

By convention there is a group with ID one at the root group, but
this is only a convention.  We need to make the group.

> import Sound.SC3

> withSC3 (send (g_new [(1,AddToTail,0)]))
