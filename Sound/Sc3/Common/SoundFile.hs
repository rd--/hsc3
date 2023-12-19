-- | Sound file functions.
module Sound.Sc3.Common.SoundFile where

import System.Environment {- base -}
import System.IO.Unsafe {- base -}
import System.Process {- process -}

import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Sound.Sc3.Common.Base {- hsc3 -}

{- | Find the file fn (case-sensitively) starting from dir.
Runs the system command "find" (so UNIX only).
Note that fn must be a file name not a relative path name.

> findFileFromDirectory "/home/rohan/data/audio" "metal.wav"
-}
findFileFromDirectory :: String -> String -> IO (Maybe String)
findFileFromDirectory dir fn = do
  r <- System.Process.readProcess "find" [dir, "-name", fn] ""
  case lines r of
    [] -> return Nothing
    [r0] -> return (Just r0)
    _ -> error "findFileFromDirectory: multiple files?"

{- | Find the file fn starting from dir.
If dir/fn names a file return that file, else call findFileFromDirectory.
Note this will not find dir/p/q/r given q/r, the query must be either p/q/r or r.

> findFileAtOrFromDirectory "/home/rohan/data/audio" "instr/bosendorfer/072/C5.aif"
-}
findFileAtOrFromDirectory :: String -> String -> IO (Maybe String)
findFileAtOrFromDirectory dir fn = do
  isAtDir <- doesFileExist (dir </> fn)
  if isAtDir then return (Just (dir </> fn)) else findFileFromDirectory dir fn

{- | Run findFileAtOrFromDirectory for each entry in path until the file is found, or not.

> findFileAtOrFromPath ["/home/rohan/rd/data/"] "20.2-LW+RD.flac"
-}
findFileAtOrFromPath :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFileAtOrFromPath path fn =
  case path of
    [] -> return Nothing
    dir : path' -> do
      m <- findFileAtOrFromDirectory dir fn
      case m of
        Just r -> return (Just r)
        Nothing -> findFileAtOrFromPath path' fn

-- | SFDIR environment variable.
sfDir :: IO FilePath
sfDir = getEnv "SFDIR"

-- | SFPATH environment variable.
sfPath :: IO FilePath
sfPath = getEnv "SFPATH"

{- | Find file fn at 'sfDir' directly or along 'sfPath'.
If fn is either absolute or names a relative file and if that file exists it is returned.
If sdDir/fn exists it is returned.
Else each directory at sfPath is searched (recursively) in turn.
Despite the name this will find any file type along the SFDIR and SFPATH, i.e. .sfz files &etc.

> mapM sfFindFile ["/home/rohan/data/audio/metal.wav", "pf-c5.aif", "20.2-LW+RD.flac"]
-}
sfFindFile :: FilePath -> IO (Maybe FilePath)
sfFindFile fn = do
  dir <- sfDir
  path <- fmap (Sound.Sc3.Common.Base.string_split_at_char ':') sfPath
  isFn <- doesFileExist fn
  isDir <- doesFileExist (dir </> fn)
  if isFn then return (Just fn) else if isDir then return (Just (dir </> fn)) else findFileAtOrFromPath path fn

-- | sfFindFile or error.
sfResolveFile :: FilePath -> IO FilePath
sfResolveFile fn = do
  m <- sfFindFile fn
  case m of
    Nothing -> error ("sfResolveFile: " ++ fn)
    Just r -> return r

{- | Unsafe sfResolveFile.
For resolving sound file names at read only sound file archives this is quite safe.
-}
sfResolve :: FilePath -> FilePath
sfResolve = unsafePerformIO . sfResolveFile

{- | Return the number of channels at fn.
Runs the system command "soxi" (so UNIX only).

> sfResolveFile "metal.wav" >>= sfNumChannels >>= print
-}
sfNumChannels :: FilePath -> IO Int
sfNumChannels fn = fmap read (System.Process.readProcess "soxi" ["-c", fn] "")

{- | Return the sample rate of fn.
Runs the system command "soxi" (so UNIX only).

> sfResolveFile "metal.wav" >>= sfSampleRate >>= print
-}
sfSampleRate :: FilePath -> IO Int
sfSampleRate fn = fmap read (System.Process.readProcess "soxi" ["-r", fn] "")

{- | Return the number of frames at fn.
Runs the system command "soxi" (so UNIX only).

> sfResolveFile "metal.wav" >>= sfFrameCount >>= print
-}
sfFrameCount :: FilePath -> IO Int
sfFrameCount fn = fmap read (System.Process.readProcess "soxi" ["-s", fn] "")

{- | Return the number of channels, sample-rate and number of frames at fn.
Runs the system command "soxi" (so UNIX only).

> sfResolveFile "metal.wav" >>= sfMetadata >>= print
-}
sfMetadata :: FilePath -> IO (Int, Int, Int)
sfMetadata fn = do
  nc <- sfNumChannels fn
  sr <- sfSampleRate fn
  nf <- sfFrameCount fn
  return (nc, sr, nf)

{- | Unsafe sfMetadata.
For fetching sound file information from read only sound file archives this is quite safe.

> sfInfo (sfResolve "metal.wav") == (1,44100,1029664)
> sfInfo (sfResolve "pf-c5.aif") == (2,44100,576377)
-}
sfInfo :: FilePath -> (Int, Int, Int)
sfInfo = unsafePerformIO . sfMetadata
