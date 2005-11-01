module Hsc.Udp where

import Network.Socket

import Hsc.U8v
import Hsc.OpenSoundControl

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

sync' :: Socket -> String -> U8v -> IO Bool
sync' fd rply b = do send' fd b
                     r <- recv' fd
                     return (rply == (fst (unosc r)))
