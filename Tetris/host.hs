module Host where
import Network
import System.IO
import Control.Concurrent
import Control.Applicative

main :: IO ()
main = withSocketsDo $ do--For windows compatibility
    do
       theSocket <- listenOn (PortNumber 25565)
       connection1 <- accept theSocket
       connection2 <- accept theSocket
       let (handle1, hostname1, portnumber1) = connection1
       let (handle2, hostname2, portnumber2) = connection2
       putStrLn "Accepted 2 connections! Multiplayer!"
       
       input1 <- lines <$> hGetContents handle1
       input2 <- lines <$> hGetContents handle2
       
       hPutStrLn handle1 "OK"
       hPutStrLn handle2 "OK"
       
       forkIO $ mapM_ (\k -> do
                                --putStrLn ("Client1 sent to Client2: " ++ k)
                                hPutStrLn handle2 k
                                hFlush handle2) input1
       mapM_ (\k -> do
                                --putStrLn ("Client2 sent to Client1: " ++ k)
                                hPutStrLn handle1 k
                                hFlush handle1) input2
       hClose handle1
       hClose handle2
       putStrLn "Connection interrupted?"
       main
