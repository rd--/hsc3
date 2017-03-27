import Control.Exception {- base -}
import Control.Monad {- base -}
import qualified Data.Tree as T {- containers -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import qualified Sound.File.NeXT as SF {- hsc3-sf -}

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

buffer_store :: Int -> FilePath -> IO ()
buffer_store n fn = do
  ((_,nf,nc,sr),d) <- withSC3 (b_fetch_hdr 512 n)
  let hdr = SF.Header nf SF.Float (round sr) nc
  SF.au_write fn hdr d

buffer_store_seq :: Int -> Double -> Bool -> FilePath -> IO ()
buffer_store_seq n dt iso dir = do
  let run = do t <- time
               let t' = if iso then time_pp t else show (ntpr_to_ntpi t)
                   fn = dir </> t' <.> "au"
               buffer_store n fn
               pauseThread dt
  forever run

-- > buffer_free_range 0 100
buffer_free_range :: Int -> Int -> IO ()
buffer_free_range b0 bN = withSC3 (mapM_ (\n -> async (b_free n)) [b0 .. bN])

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

wait_for :: IO ()
wait_for = do
  let w = pauseThread (0.25::Double)
      f = withSC3_ (send (c_get [0]) >> waitReply "/c_set")
      g e = print ("wait_for: retry",e::IOError) >> w >> h
      h = catch f g
  putStrLn "wait_for: begin" >> h >> putStrLn "wait_for: end"

help :: [String]
help =
    ["buffer query id:int"
    ,"buffer store id:int au-file:string"
    ,"buffer store-seq id:int dt:float iso|ntpi dir:string"
    ,"buffer free-range b0:int bN:int"
    ,"group query-tree id:int"
    ,"node query id:int"
    ,"reset"
    ,"status"
    ,"wait-for"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["buffer","query",n] -> buffer_query (read n)
    ["buffer","store",n,fn] -> buffer_store (read n) fn
    ["buffer","store-seq",n,dt,ts,dir] -> buffer_store_seq (read n) (read dt) (ts == "iso") dir
    ["buffer","free-range",b0,bN] -> buffer_free_range (read b0) (read bN)
    ["group","query-tree",n] -> group_query_tree (read n)
    ["node","query",n] -> node_query (read n)
    ["reset"] -> withSC3 reset
    ["status"] -> withSC3 serverStatus >>= mapM_ putStrLn
    ["wait-for"] -> wait_for
    _ -> putStrLn (unlines help)
