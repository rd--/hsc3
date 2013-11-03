-- | Core non-type variant constructors.
module Sound.SC3.Server.Command.Core where

import Sound.OSC.Core {- hosc -}

import Sound.SC3.Server.Enum
import Sound.SC3.Server.Synthdef

-- * Instrument definition commands

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: Synthdef -> Message
d_recv d = message "/d_recv" [Blob (synthdefData d)]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -> Message
d_load p = message "/d_load" [string p]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -> Message
d_loadDir p = message "/d_loadDir" [string p]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -> Message
d_free = message "/d_free" . map string

-- * Plugin commands

-- | Send a plugin command.
cmd :: String -> [Datum] -> Message
cmd name = message "/cmd" . (string name :)

-- * Server operation commands

-- | Remove all bundles from the scheduling queue.
clearSched :: Message
clearSched = message "/clearSched" []

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: PrintLevel -> Message
dumpOSC c = message "/dumpOSC" [int32 (fromEnum c)]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool -> Message
notify c = message "/notify" [int32 (fromEnum c)]

-- | Stop synthesis server.
quit :: Message
quit = message "/quit" []

-- | Request \/status.reply message.
status :: Message
status = message "/status" []

-- | Set error posting scope and mode.
errorMode :: ErrorScope -> ErrorMode -> Message
errorMode scope mode =
    let e = case scope of
              Globally -> fromEnum mode
              Locally  -> -1 - fromEnum mode
    in message "/error" [int32 e]

-- | End real time mode, close file (un-implemented).
nrt_end :: Message
nrt_end = message "/nrt_end" []

-- * Modify existing message to include completion message

-- | List of asynchronous server commands.
async_cmds :: [String]
async_cmds =
    ["/b_alloc"
    ,"/b_allocRead"
    ,"/b_allocReadChannel"
    ,"/b_close"
    ,"/b_free"
    ,"/b_read"
    ,"/b_readChannel"
    ,"/b_write"
    ,"/b_zero"
    ,"/d_load"
    ,"/d_loadDir"
    ,"/d_recv"
    ,"/notify"
    ,"/quit"
    ,"/sync"]

-- | 'True' if 'Message' is an asynchronous 'Message'.
--
-- > map isAsync [b_close 0,n_set1 0 "0" 0] == [True,False]
isAsync :: Message -> Bool
isAsync (Message a _) = a `elem` async_cmds

-- | Add a completion message (or bundle, the name is misleading) to
-- an existing asynchronous command.
--
-- > let {m = n_set1 0 "0" 0
-- >     ;m' = encodeMessage m}
-- > in withCM (b_close 0) m == Message "/b_close" [Int 0,Blob m']
withCM :: OSC o => Message -> o -> Message
withCM (Message c xs) cm =
    if c `elem` async_cmds
    then let xs' = xs ++ [Blob (encodeOSC cm)]
         in Message c xs'
    else error ("withCM: not async: " ++ c)

