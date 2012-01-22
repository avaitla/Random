module SSHTunnel where

import Network.Socket
import Network.BSD
import Network.SocketServer
import Network.Socket.Internal

import System.Event
import System.Posix.IO

import Control.Applicative ((<$>))
import Data.Set

class SSHTransport a where
    openChannel     :: String -> (String, PortNumber) -> String -> a -> IO Socket
    tunnelf         :: PortNumber -> PortNumber -> String -> String -> a -> IO Tunnel 
    shutDown        :: a -> IO Bool
    
data OpenSSH = OpenSSH { 
    server  :: String,
    keyfile :: Maybe String,
    pid     :: String
}    


forwardTunnel :: SSHTransport a => PortNumber -> String -> PortNumber -> a -> IO ()
forwardTunnel local_port remote_host remote_port transport = service where
    service = serveTCPforever inetOpts (threadedHandler handler) 
    inetOpts = simpleTCPOptions local_port { reuse = True }
    handler hdl _remote_addr _local_addr = do
        sshSock <- openChannel "Direct-tcpip" (remote_host, remote_port) _remote_addr
        clientFD <- handleToFd hdl

        mgr <- new
        let ssh_fd = fromIntegral (fdSocket sshSock)
            callback socker key evtRead = do
            (result, strlen) <- fdRead 1024 socker
            if strlen == 0
                then shutdown mgr
                else fdWrite ssh_fd result
        
        registerFd mgr (callback clientFD) clientFD evtRead
        registerFd mgr (callback ssh_fd) ssh_fd evtRead
        loop mgr

tunnelConnection :: SSHTransport a => Socket -> String -> String -> a -> IO Tunnel
tunnelConnection localSock seenByRemote sshLogin tunnelType = connect localSock newURL >> return tunnel
    where (newURL, tunnel) = openTunnel seenByRemote sshLogin tunnelType

openTunnel :: SSHTransport a => String -> String -> a -> IO (String, Tunnel)
openTunnel seenByRemote sshLogin tunnelType = do
    lport <- head <$> selectRandomPorts 1
    let [transport, addr] = split "://" seenByRemote
        [ip, _rport] = split ":" addr
        rport = read _rport :: Int

    tunnel <- tunnelf lport rport sshLogin ip a
    return (printf "tcp://127.0.0.1:%d" lport, tunnel)



splitServer :: String -> Maybe (String, String, Int)
splitServer str = result where
    result = if (length sp1 == 2 && length sp2 == 2)
        then Just (sp1 ! 0, sp2 ! 0, read sp2 ! 1 :: Int
        else Nothing
    sp2 = split ":" (sp1 ! 1)
    sp1 = split "@" str

selectRandomPorts :: Int -> IO [PortNumber]
selectRandomPorts i = _sel i empty where
    _sel :: Int -> Set PortNumber -> IO [PortNumber]
    _sel i st | i < 0     = error "Must be Non-zero"
              | i == 0    = return $ toList st
              | otherwise = do
    sock <- socket AF_INET Datagram defaultProtocol
    addr <- hostAddress . getHostByName <$> getHostName ""
    bindSocket sock (SockAddrInet 0 addr)
    (SockAddrInet port_num _) <- getSocketName sock
    sClose sock
    if member port_num st
        then _sel i st
        else _sel (i - 1) (insert port_num st)