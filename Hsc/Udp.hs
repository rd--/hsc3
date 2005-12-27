module Hsc.Udp where

import Hsc.U8v
import Hsc.OpenSoundControl

import Network.Socket

sc :: IO Socket
sc = do fd <- socket AF_INET Datagram 0
        a <- inet_addr "127.0.0.1"
        connect fd (SockAddrInet 57110 a)
        -- setSocketOption fd RecvTimeOut 1000
        return fd

send' :: Socket -> U8v -> IO Int
send' fd b = send fd (u8v_str b)

recv' :: Socket -> IO U8v
recv' fd   = do b <- recv fd 8192
                return (str_u8v b)

sync' :: Socket -> U8v -> IO OscM
sync' fd b = do send' fd b
                r <- recv' fd
                return (unosc r)

syncto' :: Socket -> String -> U8v -> IO Bool
syncto' fd rpl b = do (c,_) <- sync' fd b
                      return (c == rpl)

close' :: Socket -> IO ()
close' = sClose
