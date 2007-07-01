module Sound.SC3.Server.Private where

-- * Local utility functions.

mkDuples :: (a -> c) -> (b -> c) -> [(a, b)] -> [c]
mkDuples a b = concatMap (\(x,y) -> [a x, b y])

mkTriples :: (a -> d) -> (b -> d) -> (c -> d) -> [(a, b, c)] -> [d]
mkTriples a b c = concatMap (\(x,y,z) -> [a x, b y, c z])

