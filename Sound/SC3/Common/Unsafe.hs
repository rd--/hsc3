-- | Unsafe variants of monadic functions.
module Sound.SC3.Common.Unsafe where

import System.IO.Unsafe {- base -}

-- * Lift

-- | Lift monadic r to unsafe form.
liftUnsafe0 :: (IO r) -> (x -> r)
liftUnsafe0 fn _ = unsafePerformIO fn

-- | Lift monadic r to unsafe form.
liftUnsafe1 :: (a -> IO r) -> (a -> r)
liftUnsafe1 fn = unsafePerformIO . fn

-- | Lift monadic r to unsafe form.
liftUnsafe2 :: (a -> b -> IO r) -> (a -> b -> r)
liftUnsafe2 fn a = unsafePerformIO . fn a

-- | Lift monadic r to unsafe form.
liftUnsafe3 :: (a -> b -> c -> IO r) -> (a -> b -> c -> r)
liftUnsafe3 fn a b = unsafePerformIO . fn a b

-- | Lift monadic r to unsafe form.
liftUnsafe4 :: (a -> b -> c -> d -> IO r) -> (a -> b -> c -> d -> r)
liftUnsafe4 fn a b c = unsafePerformIO . fn a b c

-- | Lift monadic r to unsafe form.
liftUnsafe5 :: (a -> b -> c -> d -> e -> IO r) -> (a -> b -> c -> d -> e -> r)
liftUnsafe5 fn a b c d = unsafePerformIO . fn a b c d

-- | Lift monadic r to unsafe form.
liftUnsafe6 :: (a -> b -> c -> d -> e -> f -> IO r) -> (a -> b -> c -> d -> e -> f -> r)
liftUnsafe6 fn a b c d e = unsafePerformIO . fn a b c d e

-- | Lift monadic r to unsafe form.
liftUnsafe7 :: (a -> b -> c -> d -> e -> f -> g -> IO r) -> (a -> b -> c -> d -> e -> f -> g -> r)
liftUnsafe7 fn a b c d e f = unsafePerformIO . fn a b c d e f

-- | Lift monadic r to unsafe form.
liftUnsafe8 :: (a -> b -> c -> d -> e -> f -> g -> h -> IO r) -> (a -> b -> c -> d -> e -> f -> g -> h -> r)
liftUnsafe8 fn a b c d e f g = unsafePerformIO . fn a b c d e f g

-- | Lift monadic r to unsafe form.
liftUnsafe9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> IO r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
liftUnsafe9 fn a b c d e f g h = unsafePerformIO . fn a b c d e f g h

-- | Lift monadic r to unsafe form.
liftUnsafe10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> r)
liftUnsafe10 fn a b c d e f g h i = unsafePerformIO . fn a b c d e f g h i

-- | Lift monadic r to unsafe form.
liftUnsafe11 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> IO r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> r)
liftUnsafe11 fn a b c d e f g h i j = unsafePerformIO . fn a b c d e f g h i j

-- | Lift monadic r to unsafe form.
liftUnsafe12 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> IO r) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> r)
liftUnsafe12 fn a b c d e f g h i j k = unsafePerformIO . fn a b c d e f g h i j k
