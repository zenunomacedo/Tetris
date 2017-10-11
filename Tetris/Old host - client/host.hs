module Host where
import Network
import System.IO
import Control.Concurrent

main :: IO ()
main = withSocketsDo $ --For windows compatibility
    do
       theSocket <- listenOn (PortNumber 25565)
       sequence_ $ repeat $ acceptConnectionAndFork theSocket
       
       
acceptConnectionAndFork :: Socket -> IO ()
acceptConnectionAndFork theSocket = do
    connection <- accept theSocket
    let (handle, hostname, portnumber) = connection
    putStr ( "(" ++ hostname ++ ":" ++ (show portnumber) ++ "): Open\n" )
    forkIO (echoServer connection)
    return ()
    
echoServer :: (Handle, String, PortNumber) -> IO ()
echoServer (handle, hostname, portnumber) = do
    a <- hGetContents handle
    putStr $ foldr (++) "" $ map (\a -> "(" ++ hostname ++ ":" ++ (show portnumber) ++ "): Msg " ++ (show a) ++ "\n") $ lines a
    putStr ( "(" ++ hostname ++ ":" ++ (show portnumber) ++ "): Close\n" )