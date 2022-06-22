-- | Multiple channel expansion
module Sound.Sc3.Ugen.Mce where

import Control.Monad {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import Sound.Sc3.Common.Uid {- hsc3 -}
import qualified Sound.Sc3.Ugen.Math as Math {- hsc3 -}
import Sound.Sc3.Ugen.Type {- hsc3 -}

-- | Construct a list of Ugens by applying f to consecutive identifiers (from z) and indices (from 0).
listFillId :: (Integral n, ID z, Enum z) => z -> Int -> (z -> n -> Ugen) -> [Ugen]
listFillId z n f = zipWith f [z..] [0 .. fromIntegral n - 1]

-- | Construct a list of Ugens by applying f at consecutive indices (from 0).
listFillM :: (Uid m, Integral n) => Int -> (n -> m Ugen) -> m [Ugen]
listFillM n f = mapM f [0 .. fromIntegral n - 1]

-- | Construct a list of Ugens by applying f at consecutive indices (from 0).
listFill :: Integral n => Int -> (n -> Ugen) -> [Ugen]
listFill n f = map f [0 .. fromIntegral n - 1]

-- | 'mce' of 'replicate'
mceConst :: Int -> Ugen -> Ugen
mceConst n = mce . replicate n

-- | 'mce' of 'map' /f/ of 'id_seq' /n/.
mceGenId :: ID z => (Id -> Ugen) -> Int -> z -> Ugen
mceGenId f n = mce . map f . id_seq n

-- | Applicative variant of mceGenId.
mceGenM :: Applicative f => f Ugen -> Int -> f Ugen
mceGenM f n = fmap mce (replicateM n f)

-- | Count 'mce' channels.
mceSize :: Ugen -> Ugen
mceSize = constant . length . mceChannels

-- | Mix divided by number of inputs.
mceMean :: Ugen -> Ugen
mceMean e = let p = mceChannels e in Math.sum_opt p / constant (length p)

-- | Construct an Mce array of Ugens.
mceFill :: Integral n => Int -> (n -> Ugen) -> Ugen
mceFill n = mce . listFill n

-- | 'mce' of 'listFillId'
mceFillId :: (Integral n, ID z, Enum z) => z -> Int -> (z -> n -> Ugen) -> Ugen
mceFillId z n = mce . listFillId z n

-- | Type specialised mceFill
mceFillInt :: Int -> (Int -> Ugen) -> Ugen
mceFillInt = mceFill

-- | Collapse possible mce by summing.
mix :: Ugen -> Ugen
mix = Math.sum_opt . mceChannels

-- | Mix variant, sum to n channels.
mixTo :: Int -> Ugen -> Ugen
mixTo n u =
    let xs = transpose (Split.chunksOf n (mceChannels u))
    in mce (map Math.sum_opt xs)

-- | 'mix' of 'mceFill'
mixFill :: Integral n => Int -> (n -> Ugen) -> Ugen
mixFill n = mix . mceFill n

-- | Type specialised mixFill
mixFillInt :: Int -> (Int -> Ugen) -> Ugen
mixFillInt = mixFill

-- | Type specialised mixFill
mixFillUgen :: Int -> (Ugen -> Ugen) -> Ugen
mixFillUgen = mixFill

-- | 'mix' of 'mceFillId'
mixFillId :: (Integral n, ID z, Enum z) => z -> Int -> (z -> n -> Ugen) -> Ugen
mixFillId z n = mix . mceFillId z n

-- | Monad variant on mixFill.
mixFillM :: (Integral n, Monad m) => Int -> (n -> m Ugen) -> m Ugen
mixFillM n f = fmap Math.sum_opt (mapM f [0 .. fromIntegral n - 1])
