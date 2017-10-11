module Client where
import Network
import System.IO
import Control.Concurrent

main :: IO ()
main = withSocketsDo $ --For windows compatibility
    do
       handle <- connectTo "localHost" (PortNumber 25565)
       input <- getContents
       forkIO $ sequence_ $ map (\a -> do
                                hPutStr handle $ a ++ "\n"
                                hFlush handle ) $ lines input
       output <- hGetContents handle
       sequence_ $ map (putStrLn) $ lines output
       hClose handle