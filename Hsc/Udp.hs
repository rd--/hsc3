module Hsc.Udp where

import Hsc.U8v (u8v_str, str_u8v)
import Hsc.OpenSoundControl (Osc, osc, unosc)

import Network.Socket

sc :: IO Socket
sc = do fd <- socket AF_INET Datagram 0
        a <- inet_addr "127.0.0.1"
        connect fd (SockAddrInet 57110 a)
        -- setSocketOption fd RecvTimeOut 1000
        return fd

send' :: Socket -> Osc -> IO Int
send' fd o = send fd (u8v_str (osc o))

recv' :: Socket -> IO Osc
recv' fd   = do b <- recv fd 8192
                return (unosc (str_u8v b))

sync' :: Socket -> Osc -> IO Osc
sync' fd o = do send' fd o
                recv' fd

{-
syncto' :: Socket -> String -> Osc -> IO Bool
syncto' fd rpl o = do (OscM c _) <- sync' fd o
                      return (c == rpl)
-}

close' :: Socket -> IO ()
close' = sClose
