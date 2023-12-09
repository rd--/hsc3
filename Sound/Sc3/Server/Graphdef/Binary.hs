-- | Binary encoders and decoders.
module Sound.Sc3.Server.Graphdef.Binary where

import System.FilePath {- filepath -}

import qualified Data.Binary.Get as Get {- binary -}
import qualified Data.Binary.IEEE754 as IEEE754 {- data-binary-ieee754 -}
import qualified Data.ByteString.Lazy as ByteString {- bytestring -}

import qualified Sound.Osc.Coding.Byte as Byte {- hosc -}
import qualified Sound.Osc.Coding.Cast as Cast {- hosc -}
import qualified Sound.Osc.Datum as Datum {- hosc -}

import Sound.Sc3.Server.Graphdef {- hsc3 -}

-- * Binary Get (version 0|1 or 2)

-- | Get a 'Name' (Pascal string).
get_pstr :: Get.Get Name
get_pstr = do
  n <- fmap fromIntegral Get.getWord8
  fmap Byte.decode_ascii (Get.getLazyByteString n)

-- | Get_Functions for binary .scsyndef files.
binary_get_f :: Get_Functions Get.Get
binary_get_f =
  (get_pstr
  ,fmap fromIntegral Get.getInt8
  ,fmap fromIntegral Get.getInt16be
  ,fmap fromIntegral Get.getInt32be
  ,fmap realToFrac IEEE754.getFloat32be)

-- * Read

{- | Read Graphdef from .scsyndef file.

> dir = "/home/rohan/sw/rsc3-disassembler/scsyndef/"
> pp nm = read_graphdef_file (dir ++ nm) >>= putStrLn . graphdef_stat
> pp "simple.scsyndef"
> pp "with-ctl.scsyndef"
> pp "mce.scsyndef"
> pp "mrg.scsyndef"
-}
read_graphdef_file :: FilePath -> IO Graphdef
read_graphdef_file nm = do
  b <- ByteString.readFile nm
  return (Get.runGet (get_graphdef binary_get_f) b)

-- * Stat

-- | 'read_graphdef_file' and run 'graphdef_stat'.
scsyndef_stat :: FilePath -> IO String
scsyndef_stat fn = do
  g <- read_graphdef_file fn
  return (graphdef_stat g)

-- * Encode (version zero)

-- | 'Encode_Functions' for 'ByteString.ByteString'
enc_bytestring :: Encode_Functions ByteString.ByteString
enc_bytestring =
  (ByteString.concat,encode_pstr,Byte.encode_i8,Byte.encode_i16,Byte.encode_i32,encode_sample
  ,const ByteString.empty)


{- | Pascal (length prefixed) encoding of 'Name'.

> ByteString.unpack (encode_pstr (ascii "string")) ==  [6, 115, 116, 114, 105, 110, 103]
-}
encode_pstr :: Name -> ByteString.ByteString
encode_pstr = ByteString.pack . Cast.str_pstr . Datum.ascii_to_string


-- | Byte-encode 'Input'.
encode_input :: Input -> ByteString.ByteString
encode_input = encode_input_f enc_bytestring

-- | Byte-encode 'Control'.
encode_control :: Control -> ByteString.ByteString
encode_control = encode_control_f enc_bytestring

-- | Byte-encode 'Ugen'.
encode_ugen :: Ugen -> ByteString.ByteString
encode_ugen = encode_ugen_f enc_bytestring

-- | Encode 'Sample' as 32-bit IEEE float.
encode_sample :: Sample -> ByteString.ByteString
encode_sample = Byte.encode_f32 . realToFrac

-- | Encode 'Graphdef'.
encode_graphdef :: Graphdef -> ByteString.ByteString
encode_graphdef = encode_graphdef_f enc_bytestring


-- * IO

-- | Write 'Graphdef' to indicated file.
graphdefWrite :: FilePath -> Graphdef -> IO ()
graphdefWrite fn = ByteString.writeFile fn . encode_graphdef

-- | Write 'Graphdef' to indicated directory.  The filename is the
-- 'graphdef_name' with the appropriate extension (@scsyndef@).
graphdefWrite_dir :: FilePath -> Graphdef -> IO ()
graphdefWrite_dir dir s =
    let fn = dir </> Datum.ascii_to_string (graphdef_name s) <.> "scsyndef"
    in graphdefWrite fn s
