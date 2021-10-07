-- | Sound file functions.
module Sound.SC3.Common.SoundFile where

import System.Directory {- directory -}
import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.IO.Unsafe {- base -}
import System.Process {- process -}

import qualified Data.List.Split as Split {- split -}

-- | SFDIR environment variable.
sfDir :: IO FilePath
sfDir = getEnv "SFDIR"

-- | SFPATH environment variable.
sfPath :: IO FilePath
sfPath = getEnv "SFPATH"

{- | Run the system process "find" to find the file fn (case-sensitively) starting from dir.
     Runs the system command "find" (so UNIX only).

> findFileFromDirectory "/home/rohan/data/audio" "metal.wav"
-}
findFileFromDirectory :: String -> String -> IO (Maybe String)
findFileFromDirectory dir fn = do
  r <- System.Process.readProcess "find" [dir, "-name", fn] ""
  case lines r of
    [] -> return Nothing
    [r0] -> return (Just r0)
    _ -> error "findFileFromDirectory: multiple files?"

{- | Run findFileFromDirectory for each entry in path until the file is found, or not.

> findFileFromPath ["/home/rohan/rd/data/"] "20.2-LW+RD.flac"
-}
findFileFromPath :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFileFromPath path fn =
  case path of
    [] -> return Nothing
    dir:path' -> do
      m <- findFileFromDirectory dir fn
      case m of
        Just r -> return (Just r)
        Nothing -> findFileFromPath path' fn

{- | Find file fn at 'sfDir' directly or along 'sfPath'.
     If fn is either an absolute or names a relative file and if that file exists it is returned.
     If sdDir/fn exists it is returned.
     Else each directory at sfPath is searched (recursively) in turn.

> mapM sfFindFile ["/home/rohan/data/audio/metal.wav", "pf-c5.aif", "20.2-LW+RD.flac"]
-}
sfFindFile :: FilePath -> IO (Maybe FilePath)
sfFindFile fn = do
  dir <- sfDir
  path <- fmap (Split.splitOn ":") sfPath
  isFn <- doesFileExist fn
  isDir <- doesFileExist (dir </> fn)
  if isFn then return (Just fn) else if isDir then return (Just (dir </> fn)) else findFileFromPath path fn

-- | sfFindFile or error.
sfRequireFile :: FilePath -> IO FilePath
sfRequireFile fn = do
  m <- sfFindFile fn
  case m of
    Nothing  -> error ("sfRequireFile: " ++ fn)
    Just r -> return r

{- | Unsafe sfRequireFile.
     It's useful to refer to a soundfile by it's name.
-}
sfRequire :: FilePath -> FilePath
sfRequire = unsafePerformIO . sfRequireFile

{- | Return the number of channels at fn.
     Runs the system command "soxi" (so UNIX only).

> sfRequireFile "metal.wav" >>= sfNumChannels >>= print
-}
sfNumChannels :: FilePath -> IO Int
sfNumChannels fn = fmap read (System.Process.readProcess "soxi" ["-c", fn] "")

{- | Return the sample rate of fn.
     Runs the system command "soxi" (so UNIX only).

> sfRequireFile "metal.wav" >>= sfSampleRate >>= print
-}
sfSampleRate :: FilePath -> IO Int
sfSampleRate fn = fmap read (System.Process.readProcess "soxi" ["-r", fn] "")

{- | Return the number of frames at fn.
     Runs the system command "soxi" (so UNIX only).

> sfRequireFile "metal.wav" >>= sfFrameCount >>= print
-}
sfFrameCount :: FilePath -> IO Int
sfFrameCount fn = fmap read (System.Process.readProcess "soxi" ["-s", fn] "")

{- | Return the number of channels, sample-rate and number of frames at fn.
     Runs the system command "soxi" (so UNIX only).

> sfRequireFile "metal.wav" >>= sfInfo >>= print
-}
sfInfo :: FilePath -> IO (Int, Int, Int)
sfInfo fn = do
  nc <- sfNumChannels fn
  sr <- sfSampleRate fn
  nf <- sfFrameCount fn
  return (nc, sr, nf)
