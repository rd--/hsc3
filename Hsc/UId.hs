module Hsc.UId where

data UId      = UId Int deriving (Eq, Show)

ulift1 f (UId n)         = UId (f n)
ulift2 f (UId a) (UId b) = UId (f a b)

instance Num UId where
    negate         = ulift1 negate
    (+)            = ulift2 (+)
    (-)            = ulift2 (-)
    (*)            = ulift2 (*)
    abs            = ulift1 abs
    signum         = ulift1 signum
    fromInteger a  = UId (fromInteger a)
