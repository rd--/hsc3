-- | Variants and extensions of buffer commands defined in
-- "Sound.SC3.Server.Command".
module Sound.SC3.Server.Buffer where

-- | Segment a request for /m/ places into sets of at most /n/.
--
-- > b_segment 1024 2056 == [8,1024,1024]
-- > b_segment 1 5 == replicate 5 1
b_segment :: Int -> Int -> [Int]
b_segment n m =
    let (q,r) = m `quotRem` n
        s = replicate q n
    in if r == 0 then s else r : s

-- | Variant of 'b_segment' that takes a starting index and returns
-- /(index,size)/ duples.
--
-- > b_indices 1 5 0 == zip [0..4] (replicate 5 1)
-- > b_indices 1024 2056 16 == [(16,8),(24,1024),(1048,1024)]
b_indices :: Int -> Int -> Int -> [(Int,Int)]
b_indices n m k =
    let dx_d = scanl1 (+)
        s = b_segment n m
        i = 0 : dx_d s
    in zip (map (+ k) i) s

-- Local Variables:
-- truncate-lines:t
-- End:
