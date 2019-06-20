    Sound.SC3.Lang.Help.viewServerHelp "/g_queryTree"

> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}
> import qualified Data.Tree as T {- containers -}

    withSC3 serverTree >>= mapM_ putStrLn

> g_00 =
>   let f = control KR "freq" 440
>   in saw AR f * 0.05

> d_01 = synthdef "saw" (out 0 g_00)

> m_02 = [d_recv d_01,g_new [(100,AddToTail,1)],s_new0 "saw" 1000 AddToTail 100]

    > withSC3 (mapM_ maybe_async m_02)

> run_query_tree = withSC3 (g_queryTree1_unpack 0)

    > qt <- run_query_tree
    > print qt
    > withSC3 (sendMessage (g_dumpTree [(0,True)]))
    > queryNode_to_group_seq qt

There is support for extracting the node tree into the standard haskell
tree data type.

    > r_tr = queryTree_rt qt
    > putStrLn (unlines ["::TREE::",T.drawTree (fmap query_node_pp r_tr)])

> q_03 =
>   [int32 1,int32 0,int32 2,int32 1,int32 1
>   ,int32 100,int32 1
>   ,int32 1000,int32 (-1),string "saw"
>   ,int32 1,string "freq",float 440.0
>   ,int32 2,int32 0]

> t_04 = queryTree q_03

> t_05 = queryTree_rt t_04

    >> putStrLn (unlines ["::TREE::",T.drawTree (fmap query_node_pp t_05)])
