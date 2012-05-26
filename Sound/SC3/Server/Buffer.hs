-- | Variants and extensions of buffer commands defined in
-- "Sound.SC3.Server.Command".
module Sound.SC3.Server.Buffer where

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
b_getn1_data :: Transport t => t -> Int -> (Int,Int) -> IO [Double]
b_getn1_data fd b s = do
  let f d = case d of
              Int _:Int _:Int _:x -> map datum_real_err x
              _ -> error "b_getn1_data"
  sendMessage fd (b_getn1 b s)
  fmap f (waitAddressDatum fd "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
b_getn1_data_segment :: Transport t => t -> Int -> Int -> (Int,Int) -> IO [Double]
b_getn1_data_segment fd n b (i,j) = do
  let ix = b_indices n j i
  d <- mapM (b_getn1_data fd b) ix
  return (concat d)

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: Transport t => t -> Int -> Int -> IO [Double]
b_fetch fd n b = do
  let f d = case d of
              [Int _,Int nf,Int nc,Float _] ->
                  let ix = (0,nf * nc)
                  in b_getn1_data_segment fd n b ix
              _ -> error "b_fetch"
  sendMessage fd (b_query1 b)
  waitAddressDatum fd "/b_info" >>= f

-- Local Variables:
-- truncate-lines:t
-- End:
