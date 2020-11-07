-- soundIn ; composite of in' and numOutputBuses
soundIn 0 * 0.5

-- soundIn ; copy input from 1 & 0 to outputs 0 & 1.
soundIn (mce2 1 0) * 0.5

-- ; io matrix (0->0,1->2,2->1,3->3)
--
--   0 1 2 3
-- 0 *
-- 1     *
-- 2   *
-- 3       *
soundIn (mce [0, 2, 1, 3])
