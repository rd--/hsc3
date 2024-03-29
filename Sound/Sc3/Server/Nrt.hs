-- | Non-realtime score generation.
module Sound.Sc3.Server.Nrt where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.IO {- base -}

import qualified Data.ByteString.Lazy as B {- bytestring -}

import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import qualified Sound.Osc.Coding.Decode.Binary as Decode {- hosc3 -}
import qualified Sound.Osc.Coding.Encode.Builder as Encode {- hosc3 -}
import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Packet {- hosc -}

-- | Encode Bundle and prefix with encoded length.
oscWithSize :: BundleOf Message -> B.ByteString
oscWithSize o =
  let b = Encode.encodeBundle o
      l = Byte.encode_i32 (fromIntegral (B.length b))
  in B.append l b

-- | An 'Nrt' score is a sequence of 'Bundle's.
newtype Nrt = Nrt {nrt_bundles :: [BundleOf Message]} deriving (Show)

{- | 'span' of 'f' of 'bundleTime'.
     Can be used to separate the /initialisation/ and /remainder/ parts of a score.
-}
nrt_span :: (Time -> Bool) -> Nrt -> ([BundleOf Message], [BundleOf Message])
nrt_span f = span (f . bundleTime) . nrt_bundles

-- | Encode an 'Nrt' score.
encodeNrt :: Nrt -> B.ByteString
encodeNrt = B.concat . map oscWithSize . nrt_bundles

{- | Write an 'Nrt' score.

> import Sound.Osc {\- hosc -\}
> import Sound.Sc3 {\- hsc3 -\}
> m1 = g_new [(1, AddToTail, 0)]
> m2 = d_recv (synthdef "sin" (out 0 (sinOsc ar 660 0 * 0.15)))
> m3 = s_new "sin" 100 AddToTail 1 []
> m4 = n_free [100]
> m5 = nrt_end
> sc = Nrt [bundle 0 [m1,m2],bundle 1 [m3],bundle 10 [m4],bundle 15 [m5]]
> writeNrt "/tmp/t.osc" sc
-}
writeNrt :: FilePath -> Nrt -> IO ()
writeNrt fn = B.writeFile fn . encodeNrt

-- | Write an 'Nrt' score to a file handle.
putNrt :: Handle -> Nrt -> IO ()
putNrt h = B.hPut h . encodeNrt

-- | Decode an 'Nrt' 'B.ByteString' to a list of 'Bundle's.
decode_nrt_bundles :: B.ByteString -> [BundleOf Message]
decode_nrt_bundles s =
  let (p, q) = B.splitAt 4 s
      n = fromIntegral (Byte.decode_i32 p)
      (r, s') = B.splitAt n q
      r' = Decode.decodeBundle r
  in if B.null s'
      then [r']
      else r' : decode_nrt_bundles s'

-- | Decode an 'Nrt' 'B.ByteString'.
decodeNrt :: B.ByteString -> Nrt
decodeNrt = Nrt . decode_nrt_bundles

{- | 'decodeNrt' of 'B.readFile'.

> readNrt "/tmp/t.osc"
-}
readNrt :: FilePath -> IO Nrt
readNrt = fmap decodeNrt . B.readFile

-- * Query

-- | Find any non-ascending sequences.
nrt_non_ascending :: Nrt -> [(BundleOf Message, BundleOf Message)]
nrt_non_ascending (Nrt b) =
  case uncons b of
    Nothing -> error "nrt_non_ascending: empty nrt"
    Just (_, t) ->
      let p = zip b t
          f (i, j) = if bundleTime i > bundleTime j then Just (i, j) else Nothing
      in mapMaybe f p
