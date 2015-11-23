import qualified Data.Tree as T {- containers -}
import System.Environment {- base -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

kv_table_pp :: [(String,String)] -> [String]
kv_table_pp tbl =
    let lm = maximum (map length (map fst tbl))
        pp (k,v) = k ++ replicate (lm - length k) ' ' ++ " : " ++ v
    in map pp tbl

-- > buffer_query 0
buffer_query :: Int -> IO ()
buffer_query n = do
  (n',nf,nc,sr) <- withSC3 (b_query1_unpack n)
  let k = map snd b_info_fields
      v = [show n',show nf,show nc,show sr]
  putStrLn (unlines (kv_table_pp (zip k v)))

-- > group_query_tree 0
group_query_tree :: Int -> IO ()
group_query_tree n = do
  r <- withSC3 (send (g_queryTree [(n,True)]) >> waitReply "/g_queryTree.reply")
  let tr = queryTree_rt (queryTree (messageDatum r))
  putStrLn (unlines ["::GROUP QUERY TREE::",T.drawTree (fmap query_node_pp tr)])

-- > node_query 1
node_query :: Int -> IO ()
node_query n = do
  r <- withSC3 (withNotifications (n_query1_unpack n))
  case r of
    Nothing -> error "node_query"
    Just r' -> let tbl = zip (map (\(_,nm,_) -> nm) n_info_fields) (map show r')
               in putStrLn (unlines (kv_table_pp tbl))

help :: [String]
help =
    ["buffer query id:int"
    ,"group query-tree id:int"
    ,"node query id:int"
    ,"reset"
    ,"status"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["buffer","query",n] -> buffer_query (read n)
    ["group","query-tree",n] -> group_query_tree (read n)
    ["node","query",n] -> node_query (read n)
    ["reset"] -> withSC3 reset
    ["status"] -> withSC3 serverStatus >>= mapM_ putStrLn
    _ -> putStrLn (unlines help)
