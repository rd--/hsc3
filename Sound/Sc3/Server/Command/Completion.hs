-- | Modify existing message to include completion message
module Sound.Sc3.Server.Command.Completion where

import Sound.Osc.Core {- hosc -}

import qualified Sound.Sc3.Server.Command.Enum as Server.Command.Enum {- hsc3 -}

-- | Add a completion packet to an existing asynchronous command.
with_completion_packet :: Message -> Packet -> Message
with_completion_packet (Message c xs) cm =
    if c `elem` Server.Command.Enum.async_cmds
    then let xs' = xs ++ [Blob (encodePacket cm)]
         in Message c xs'
    else error ("with_completion_packet: not async: " ++ c)

{- | Add a completion message to an existing asynchronous command.

>>> let m = n_set1 0 "0" 0
>>> let e = encodeMessage m
>>> withCm (b_close 0) m == Message "/b_close" [Int32 0,Blob e]
True
-}
withCm :: Message -> Message -> Message
withCm m cm = with_completion_packet m (Packet_Message cm)
