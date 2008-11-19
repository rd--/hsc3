{-# LANGUAGE TypeSynonymInstances #-}
-- | This module includes utilities for spawning an external scsynth process,
-- either for realtime or non-realtime execution.
module Sound.SC3.Server.Process (
    Verbosity(..),
    ServerOptions(..),
    defaultServerOptions,
    NRTOptions(..),
    defaultNRTOptions,
    RTOptions(..),
    defaultRTOptionsUDP,
    defaultRTOptionsTCP,
    EventHandler(..),
    defaultEventHandler,
    withSynth,
    withNRT
) where

import Sound.OpenSoundControl (Transport, TCP, UDP, openTCP, openUDP)
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Prelude hiding (catch)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode)
import System.IO (Handle, hGetLine, hIsEOF, hPutStrLn, stderr, stdout)
import System.Process (runInteractiveProcess, waitForProcess)

-- ====================================================================
-- scsynth commandline options

class CommandLine a where
    argumentList :: a -> [String]

-- | Convert a value to an option string.
class Show a => Option a where
    showOption :: a -> String
    showOption = show

-- | String options need special handling (show introduces an additional level
-- of quoting).
instance Option (String) where
    showOption = id

-- | Option instance for Int with default method implementations.
instance Option (Int)

mkOpt :: (Eq b, Option b, Show b) => String -> (a -> b) -> a -> a -> [String]
mkOpt _ f d v | (f v) == (f d) = []
mkOpt o f _ v                  = [o, showOption (f v)]

mkMaybeOpt :: (Option a, Show a) => String -> Maybe a -> [String]
mkMaybeOpt o = maybe [] ((o:) . (:[]) . showOption)

-- ====================================================================
-- * Server options

-- | Used with the 'verbosity' field in 'ServerOptions'.
data Verbosity =
    Silent
  | Quiet
  | Normal
  | Verbose
  | VeryVerbose
  | ExtremelyVerbose
  deriving (Eq, Read, Show)

-- 'Enum' instance for 'Verbosity' for conversion to a commandline option.
instance Enum (Verbosity) where
    fromEnum Silent             = -2
    fromEnum Quiet              = -1
    fromEnum Normal             =  0
    fromEnum Verbose            =  1
    fromEnum VeryVerbose        =  2
    fromEnum ExtremelyVerbose   =  4

    toEnum (-1)                 = Quiet
    toEnum 0                    = Normal
    toEnum 1                    = Verbose
    toEnum 2                    = VeryVerbose
    toEnum x | x >= 4           = ExtremelyVerbose
    toEnum _                    = Silent

-- | Specify general server options used b oth in realtime and non-realtime
-- mode.
data ServerOptions = ServerOptions {
    serverProgram               :: FilePath,    -- ^ Path to the @scsynth@ program
    numberOfControlBusChannels  :: Int,         -- ^ Number of allocated control bus channels
    numberOfAudioBusChannels    :: Int,         -- ^ Number of allocated audio bus channels
    numberOfInputBusChannels    :: Int,         -- ^ Number of physical input channels
    numberOfOutputBusChannels   :: Int,         -- ^ Number of physical output channels
    blockSize                   :: Int,         -- ^ Synthesis block size
    numberOfSampleBuffers       :: Int,         -- ^ Number of allocated sample buffers
    maxNumberOfNodes            :: Int,         -- ^ Maximum number of synthesis nodes
    maxNumberOfSynthDefs        :: Int,         -- ^ Maximum number of synth definitions
    realTimeMemorySize          :: Int,         -- ^ Realtime memory size in bytes
    numberOfWireBuffers         :: Int,         -- ^ Number of unit generator connection buffers
    numberOfRandomSeeds         :: Int,         -- ^ Number of random number generator seeds
    loadSynthDefs               :: Bool,        -- ^ If 'True', load synth definitions from /synthdefs/ directory on startup
    verbosity                   :: Verbosity    -- ^ 'Verbosity' level
} deriving (Eq, Read, Show)

-- | Default server options.
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions {
    serverProgram               = "scsynth",
    numberOfControlBusChannels  = 4096,
    numberOfAudioBusChannels    = 128,
    numberOfInputBusChannels    = 8,
    numberOfOutputBusChannels   = 8,
    blockSize                   = 64,
    numberOfSampleBuffers       = 1024,
    maxNumberOfNodes            = 1024,
    maxNumberOfSynthDefs        = 1024,
    realTimeMemorySize          = 8192,
    numberOfWireBuffers         = 64,
    numberOfRandomSeeds         = 64,
    loadSynthDefs               = True,
    verbosity                   = Normal
}

instance CommandLine (ServerOptions) where
    argumentList v = serverProgram v :
            concat [  mkOpt "-c" numberOfControlBusChannels d v
                    , mkOpt "-a" numberOfAudioBusChannels d v
                    , mkOpt "-i" numberOfInputBusChannels d v
                    , mkOpt "-o" numberOfOutputBusChannels d v
                    , mkOpt "-z" blockSize d v
                    , mkOpt "-b" numberOfSampleBuffers d v
                    , mkOpt "-n" maxNumberOfNodes d v
                    , mkOpt "-d" maxNumberOfSynthDefs d v
                    , mkOpt "-w" numberOfWireBuffers d v
                    , mkOpt "-r" numberOfRandomSeeds d v
                    , mkOpt "-D" (fromEnum . loadSynthDefs) d v
                    , mkOpt "-v" (fromEnum . verbosity) d v ]
        where d = defaultServerOptions

-- ====================================================================
-- * Realtime options

-- | Helper class for polymorphic opening of network connections.
class OpenTransport t where
    openTransport :: RTOptions t -> String -> IO t

instance OpenTransport (UDP) where
    openTransport rtOptions server = openUDP server (udpPortNumber rtOptions)

instance OpenTransport (TCP) where
    openTransport rtOptions server = openTCP server (tcpPortNumber rtOptions)

-- | Realtime server options, parameterized by the OpenSoundControl
-- 'Transport' to be used.
data RTOptions t = RTOptions {
    -- Network control
    udpPortNumber           :: Int,             -- ^ UDP port number (one of 'udpPortNumber' and 'tcpPortNumber' must be non-zero)
    tcpPortNumber           :: Int,             -- ^ TCP port number (one of 'udpPortNumber' and 'tcpPortNumber' must be non-zero)
    useZeroconf             :: Bool,            -- ^ If 'True', publish scsynth service through Zeroconf
    maxNumberOfLogins       :: Int,             -- ^ Max number of supported logins if 'sessionPassword' is set
    sessionPassword         :: Maybe String,    -- ^ Session password
    -- Audio device control
    hardwareDeviceName      :: Maybe String,    -- ^ Hardware device name (JACK client:server name on Linux)
    hardwareBufferSize      :: Int,             -- ^ Hardware buffer size (no effect with JACK)
    hardwareSampleRate      :: Int,             -- ^ Hardware buffer size (no effect with JACK)
    inputStreamsEnabled     :: Maybe Int,       -- ^ Enabled input streams (CoreAudio only)
    outputStreamsEnabled    :: Maybe Int        -- ^ Enabled output streams (CoreAudio only)
} deriving (Eq, Read, Show)

-- | Default realtime server options.
defaultRTOptions :: RTOptions t
defaultRTOptions = RTOptions {
    -- Network control
    udpPortNumber           = 0,
    tcpPortNumber           = 0,
    useZeroconf             = False,
    maxNumberOfLogins       = 16,
    sessionPassword         = Nothing,
    -- Audio device control
    hardwareDeviceName      = Nothing,
    hardwareBufferSize      = 0,
    hardwareSampleRate      = 0,
    inputStreamsEnabled     = Nothing,
    outputStreamsEnabled    = Nothing
}

-- | Default realtime server options (UDP transport).
defaultRTOptionsUDP :: RTOptions UDP
defaultRTOptionsUDP = defaultRTOptions { udpPortNumber = 57110 }

-- | Default realtime server options (TCP transport).
defaultRTOptionsTCP :: RTOptions TCP
defaultRTOptionsTCP = defaultRTOptions { tcpPortNumber = 57110 }

instance CommandLine (RTOptions t) where
    argumentList v =
        concat [ mkOpt      "-u" udpPortNumber d v
               , mkOpt      "-t" tcpPortNumber d v
               , mkOpt      "-R" (fromEnum . useZeroconf) d v
               , mkMaybeOpt "-H" $ hardwareDeviceName v
               , mkOpt      "-Z" hardwareBufferSize d v
               , mkOpt      "-S" hardwareSampleRate d v
               , mkOpt      "-l" maxNumberOfLogins d v
               , mkMaybeOpt "-p" $ sessionPassword v
               , mkMaybeOpt "-I" $ inputStreamsEnabled v
               , mkMaybeOpt "-O" $ outputStreamsEnabled v ]
        where d = defaultRTOptions

-- ====================================================================
-- * Non-Realtime options

-- | Non-realtime server options.
data NRTOptions = NRTOptions {
    commandFilePath     :: Maybe FilePath,  -- ^ Path to OSC command file ('Nothing' for stdin)
    inputFilePath       :: Maybe FilePath,  -- ^ Path to input sound file ('Nothing' for no audio input)
    outputFilePath      :: FilePath,        -- ^ Path to output sound file
    outputSampleRate    :: Int,             -- ^ Output sound file sample rate
    outputHeaderFormat  :: String,          -- ^ Output sound file header format
    outputSampleFormat  :: String           -- ^ Output sound file sample format
} deriving (Eq, Read, Show)

-- | Default non-realtime server options.
defaultNRTOptions :: NRTOptions
defaultNRTOptions = NRTOptions {
    commandFilePath         = Nothing,
    inputFilePath           = Nothing,
    outputFilePath          = "output.wav",
    outputSampleRate        = 44100,
    outputHeaderFormat      = "wav",
    outputSampleFormat      = "int16"
}

instance CommandLine (NRTOptions) where
    argumentList x =
        "-N" : map ($x) [ fromMaybe "_" . commandFilePath
                        , fromMaybe "_" . inputFilePath
                        , outputFilePath
                        , show . outputSampleRate
                        , outputHeaderFormat
                        , outputSampleFormat ]

-- ====================================================================
-- * Event handler

-- | Event handler for handling I/O with external @scsynth@ processes,
-- parameterized by the I/O handle used for sending OSC commands to the
-- server.
data EventHandler t = EventHandler {
    onPutString :: String -> IO (),     -- ^ Handle one line of normal output
    onPutError  :: String -> IO (),     -- ^ Handle one line of error output
    onBoot      :: t -> IO ()           -- ^ Executed with the OSC handle after the server has booted
}

-- | Default event handler, writing to stdout and stderr, respectively.
defaultEventHandler :: EventHandler t
defaultEventHandler = EventHandler {
    onPutString = hPutStrLn stdout,
    onPutError  = hPutStrLn stderr,
    onBoot      = const (return ())
}

-- ====================================================================
-- Process helpers

pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h = hIsEOF h >>= flip unless (hGetLine h >>= f >> pipeOutput f h)

-- ====================================================================
-- * Realtime scsynth execution

-- | Execute a realtime instance of @scsynth@ with 'Transport' t and return
-- 'ExitCode' when the process exists.
--
-- /NOTE/: When compiling executables with GHC, the @-threaded@ option should be
-- passed, otherwise the I\/O handlers will not work correctly.
withSynth :: (Transport t, OpenTransport t) =>
    ServerOptions
 -> RTOptions t
 -> EventHandler t
 -> IO ExitCode
withSynth serverOptions rtOptions handler = do
        (_, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
        forkIO $ putStdout0 hOut
        forkIO $ putStderr  hErr
        waitForProcess hProc
    where
        (exe:args) = argumentList serverOptions
                     ++ argumentList rtOptions
        putStdout0 h = do
            eof <- hIsEOF h
            unless eof $ do
                l <- hGetLine h
                if isPrefixOf "SuperCollider 3 server ready.." l
                    then do
                        onPutString handler l
                        fd <- openTransport rtOptions "127.0.0.1"
                        forkIO $ onBoot handler fd
                        -- Spawn more efficient output handler
                        forkIO $ putStdout h
                        return ()
                    else do
                        onPutString handler l
                        putStdout0 h -- recurse
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutError  handler)
    
-- ====================================================================
-- * Non-Realtime scsynth execution

-- | Execute a non-realtime instance of @scsynth@ and return 'ExitCode' when
-- the process exists.
withNRT ::
    ServerOptions
 -> NRTOptions
 -> EventHandler Handle
 -> IO ExitCode
withNRT serverOptions nrtOptions handler = do
        (hIn, hOut, hErr, hProc) <- runInteractiveProcess exe args Nothing Nothing
        forkIO $ putStdout hOut
        forkIO $ putStderr hErr
        forkIO $ onBoot handler hIn
        waitForProcess hProc
    where
        (exe:args) = argumentList serverOptions
                     ++ argumentList nrtOptions { commandFilePath = Nothing }
        putStdout = pipeOutput (onPutString handler)
        putStderr = pipeOutput (onPutError  handler)

-- EOF
