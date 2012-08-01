module Sound.SC3.Server.Buffer.Monad where

import Sound.OpenSoundControl.Transport.Monad {- hosc -}
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait.Monad

import Sound.SC3.Server.Buffer
import Sound.SC3.Server.Command

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3 (b_getn1_data 0 (0,5))
b_getn1_data :: Transport m => Int -> (Int,Int) -> m [Double]
b_getn1_data b s = do
  let f d = case d of
              Int _:Int _:Int _:x -> map datum_real_err x
              _ -> error "b_getn1_data"
  sendMessage (b_getn1 b s)
  fmap f (waitDatum "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
--
-- > withSC3 (b_getn1_data_segment 1 0 (0,5))
b_getn1_data_segment :: Transport m => Int -> Int -> (Int,Int) -> m [Double]
b_getn1_data_segment n b (i,j) = do
  let ix = b_indices n j i
  d <- mapM (b_getn1_data b) ix
  return (concat d)

-- | Variant of 'b_getn1_data_segment' that gets the entire buffer.
b_fetch :: Transport m => Int -> Int -> m [Double]
b_fetch n b = do
  let f d = case d of
              [Int _,Int nf,Int nc,Float _] ->
                  let ix = (0,nf * nc)
                  in b_getn1_data_segment n b ix
              _ -> error "b_fetch"
  sendMessage (b_query1 b)
  waitDatum "/b_info" >>= f
