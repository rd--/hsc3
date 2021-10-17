-- | Multiple channel expansion
module Sound.SC3.UGen.Mce where

import Control.Monad {- base -}
import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import Sound.SC3.Common.UId {- hsc3 -}
import qualified Sound.SC3.UGen.Math as Math {- hsc3 -}
import Sound.SC3.UGen.Type {- hsc3 -}

-- | Construct a list of UGens by applying f to consecutive identifiers (from z) and indices (from 0).
listFillId :: (Integral n, ID z, Enum z) => z -> Int -> (z -> n -> UGen) -> [UGen]
listFillId z n f = zipWith f [z..] [0 .. fromIntegral n - 1]

-- | Construct a list of UGens by applying f at consecutive indices (from 0).
listFillM :: (UId m, Integral n) => Int -> (n -> m UGen) -> m [UGen]
listFillM n f = mapM f [0 .. fromIntegral n - 1]

-- | Construct a list of UGens by applying f at consecutive indices (from 0).
listFill :: Integral n => Int -> (n -> UGen) -> [UGen]
listFill n f = map f [0 .. fromIntegral n - 1]

-- | 'mce' of 'replicate'
mceConst :: Int -> UGen -> UGen
mceConst n = mce . replicate n

-- | 'mce' of 'map' /f/ of 'id_seq' /n/.
mceGenId :: ID z => (Id -> UGen) -> Int -> z -> UGen
mceGenId f n = mce . map f . id_seq n

-- | Applicative variant of mceGenId.
mceGenM :: Applicative f => f UGen -> Int -> f UGen
mceGenM f n = fmap mce (replicateM n f)

-- | Count 'mce' channels.
mceSize :: UGen -> UGen
mceSize = constant . length . mceChannels

-- | Mix divided by number of inputs.
mceMean :: UGen -> UGen
mceMean e = let p = mceChannels e in Math.sum_opt p / constant (length p)

-- | Construct an Mce array of UGens.
mceFill :: Integral n => Int -> (n -> UGen) -> UGen
mceFill n = mce . listFill n

-- | 'mce' of 'listFillId'
mceFillId :: (Integral n, ID z, Enum z) => z -> Int -> (z -> n -> UGen) -> UGen
mceFillId z n = mce . listFillId z n

-- | Type specialised mceFill
mceFillInt :: Int -> (Int -> UGen) -> UGen
mceFillInt = mceFill

-- | Collapse possible mce by summing.
mix :: UGen -> UGen
mix = Math.sum_opt . mceChannels

-- | Mix variant, sum to n channels.
mixTo :: Int -> UGen -> UGen
mixTo n u =
    let xs = transpose (Split.chunksOf n (mceChannels u))
    in mce (map Math.sum_opt xs)

-- | 'mix' of 'mceFill'
mixFill :: Integral n => Int -> (n -> UGen) -> UGen
mixFill n = mix . mceFill n

-- | Type specialised mixFill
mixFillInt :: Int -> (Int -> UGen) -> UGen
mixFillInt = mixFill

-- | Type specialised mixFill
mixFillUGen :: Int -> (UGen -> UGen) -> UGen
mixFillUGen = mixFill

-- | 'mix' of 'mceFillId'
mixFillId :: (Integral n, ID z, Enum z) => z -> Int -> (z -> n -> UGen) -> UGen
mixFillId z n = mix . mceFillId z n

-- | Monad variant on mixFill.
mixFillM :: (Integral n, Monad m) => Int -> (n -> m UGen) -> m UGen
mixFillM n f = fmap Math.sum_opt (mapM f [0 .. fromIntegral n - 1])
