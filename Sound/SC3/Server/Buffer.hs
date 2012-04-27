-- | Variants and extensions of buffer commands defined in
-- "Sound.SC3.Server.Command".
module Sound.SC3.Server.Buffer where

import Data.Maybe
import Sound.OpenSoundControl {- hosc -}
import Sound.SC3.Server.Command

-- | Segment a request for /m/ places into sets of at most /n/.
--
-- > b_segment 1024 2056 == [8,1024,1024]
b_segment :: Int -> Int -> [Int]
b_segment n m =
    let (q,r) = m `quotRem` n
    in r : replicate q n

-- | Variant of 'b_segment' that takes a starting index and returns
-- /(index,size)/ duples.
--
-- > b_indices 1024 2056 16 == [(16,8),(24,1024),(1048,1024)]
b_indices :: Int -> Int -> Int -> [(Int,Int)]
b_indices n m k =
    let dx_d = scanl1 (+)
        s = b_segment n m
        i = 0 : dx_d s
    in zip (map (+ k) i) s

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3 (\fd -> b_getn1_data fd 0 (0,5))
b_getn1_data :: Transport t => t -> Int -> (Int,Int) -> IO (Maybe [Double])
b_getn1_data fd b s = do
  do send fd (b_getn1 b s)
     p <- recv fd
     case p of
       Left m -> let Message "/b_setn" (Int _:Int _:Int _:f) = m
                 in return (Just (map datum_real' f))
       _ -> return Nothing

-- | Variant of 'b_getn1_data' that gets segments individual 'b_getn'
-- messages to /n/ elements.
b_getn1_data_segment :: Transport t =>
                        t -> Int -> Int -> (Int,Int) -> IO [Double]
b_getn1_data_segment fd n b (i,j) = do
  let ix = b_indices n j i
  d <- mapM (b_getn1_data fd b) ix
  return (concat (catMaybes d))

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: Transport t => t -> Int -> Int -> IO [Double]
b_fetch fd n b = do
  send fd (b_query1 b)
  p <- recv fd
  case p of
    Left m -> let Message "/b_info" [Int _,Int nf,Int nc,Float _] = m
              in b_getn1_data_segment fd n b (0,nf*nc)
    _ -> error "b_get_all"
