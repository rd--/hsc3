    > Sound.SC3.Server.Help.viewServerHelp "/g_queryTree"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import qualified Data.Tree as T {- containers -}

> d_0 =
>     let f = control KR "freq" 440
>         o = saw AR f * 0.05
>     in synthdef "saw" (out 0 o)

> m_0 = [d_recv d_0,g_new [(100,AddToTail,1)],s_new0 "saw" 1000 AddToTail 100]

    > withSC3 (mapM_ maybe_async m_0)

> run_query_tree = withSC3 (g_queryTree1_unpack 0)

    > qt <- run_query_tree
    > print qt
    > withSC3 (sendMessage (g_dumpTree [(0,True)]))
    > queryNode_to_group_seq qt

There is support for extracting the node tree into the standard haskell
tree data type.

    > r_tr = queryTree_rt qt
    > putStrLn (unlines ["::TREE::",T.drawTree (fmap query_node_pp r_tr)])
