module Sound.SC3.Server.Buffer.FD where

import Sound.OpenSoundControl.Transport.FD {- hosc -}
import Sound.OpenSoundControl.Type
import Sound.OpenSoundControl.Wait.FD

import Sound.SC3.Server.Buffer
import Sound.SC3.Server.Command

-- | Variant of 'b_getn1' that waits for return message and unpacks it.
--
-- > withSC3 (\fd -> b_getn1_data fd 0 (0,5))
b_getn1_data :: Transport t => t -> Int -> (Int,Int) -> IO [Double]
b_getn1_data fd b s = do
  let f d = case d of
              Int _:Int _:Int _:x -> map datum_real_err x
              _ -> error "b_getn1_data"
  sendMessage fd (b_getn1 b s)
  fmap f (waitDatum fd "/b_setn")

-- | Variant of 'b_getn1_data' that segments individual 'b_getn'
-- messages to /n/ elements.
--
-- > withSC3 (\fd -> b_getn1_data_segment fd 1 0 (0,5))
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
  waitDatum fd "/b_info" >>= f
