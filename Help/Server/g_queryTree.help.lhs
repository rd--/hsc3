> Sound.SC3.Server.Help.viewServerHelp "/g_queryTree"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> let d = let {f = control KR "freq" 440
>             ;o = saw AR f * 0.05}
>         in synthdef "saw" (out 0 o)

> withSC3 (async (d_recv d) >>
>          send (g_new [(100,AddToTail,1)]) >>
>          send (s_new0 "saw" 1000 AddToTail 100))

> r <- withSC3 (send (g_queryTree [(0,True)]) >>
>               waitReply "/g_queryTree.reply")

> print r

> withSC3 (send (g_dumpTree [(0,True)]))

There is support for extracting the node tree into the standard haskell
tree data type.

> import qualified Data.Tree as T {- containers -}

> let tr = queryTree_rt (queryTree (messageDatum r))

> putStrLn (unlines ["::TREE::",T.drawTree (fmap query_node_pp tr)])
