module Menu where
import Graphics.UI.GLUT hiding (renderPrimitive)
import Data.IORef
import Control.Monad (when)
import Control.Concurrent (forkIO)
import System.IO
import Network
import Types
import SinglePlayer
import Multiplayer


buttonWidth = 150
buttonWidthf = fromIntegral buttonWidth


displayMenu :: IORef Everything -> IO ()
displayMenu everythingRef = do
    clear [ColorBuffer]
    renderPrimitive Quads $ do
    --fundo
        color3f 0 0 0
        vertex2 (-(widthf / 2)) (heigthf / 2)
        vertex2 (widthf / 2) (heigthf / 2)
        vertex2 (widthf / 2) (-(heigthf / 2)) 
        vertex2 (-(widthf / 2)) (-(heigthf / 2)) 
    --botão SinglePlayer
        color3f 255 255 255
        vertex2 (-buttonWidthf) (heigthf / 4) 
        vertex2 (buttonWidthf) (heigthf / 4) 
        vertex2 (buttonWidthf) (heigthf / 8)
        vertex2 (-buttonWidthf) (heigthf / 8)
     --botão MultiPlayer
        vertex2 (-buttonWidthf) (heigthf / 16)
        vertex2 (buttonWidthf) (heigthf / 16)
        vertex2 (buttonWidthf) (-heigthf / 16)
        vertex2 (-buttonWidthf) (-heigthf / 16)
     
    color3f 255 255 255
    renderText (-25,heigthf / 3) "Tetris"
    color3f 0 0 0
    renderText (-50, 3 * heigthf / 16) "Single Player"--2º parametro do tuplo é o centro do butao em y
    renderText (-50, 0) "Multiplayer"--2º parametro do tuplo é o centro do butao em y
    
    flush

clickPos :: IORef Everything -> IORef Enemy -> MouseCallback    
clickPos everythingRef enemyRef LeftButton Down (Position coordx coordy)  = do
 let (x,y) = (coordx-400, (300-coordy)) --conversao
 if (x > -buttonWidth) && (x < buttonWidth) 
   then do
    when ((y > div heigth 8) && (y < div heigth 4)) (do
                                                        modifyIORef' everythingRef (\k -> k{gameMode = SinglePlayer})
                                                        mouseCallback $= Nothing
                                                        specialCallback $= Just (move everythingRef)
                                                        specialUpCallback $= Just (releaseKey everythingRef)
                                                        keyboardCallback $= Just (dropIt everythingRef)
                                                        addTimerCallback timeWait $ redisplay everythingRef
                                                    )    
    
    
    when ((y > (-1) * (div heigth 16)) && (y < div heigth 16)) (do
                                                        modifyIORef' everythingRef (\k -> k{gameMode = MultiPlayer})
                                                        mouseCallback $= Nothing
                                                        specialCallback $= Just (move everythingRef)
                                                        specialUpCallback $= Just (releaseKey everythingRef)   
                                                        keyboardCallback $= Just (dropIt everythingRef)
                                                        addTimerCallback timeWait $ redisplayMulti everythingRef enemyRef
                                                        withSocketsDo $ do
                                                                let ip = "127.0.0.1" --"195.154.242.224" server do Arantes
                                                                handle <- connectTo ip (PortNumber 25565)
                                                                modifyIORef' enemyRef (\e -> e{getHandle = handle})
                                                                connectCheck <- hGetLine handle
                                                                --when (connectCheck /= "OK") $ error "obama" 
                                                                forkIO $ readInfo everythingRef enemyRef
                                                                return ()
                                                        )
   else return ()
 
clickPos _ _ _ _ _ = return ()
 
 
 
 
 
 
 