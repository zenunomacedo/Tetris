module Main where
import Graphics.UI.GLUT hiding (Red, Blue, Green, Menu, renderPrimitive) --graficos
import Data.IORef       --variaveis globais
import SinglePlayer 
import Multiplayer
import Types
import Menu



 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize --inicializar
  _window <- createWindow "Tetris" --criar window
  windowSize $= Size width heigth --tamanho da window definido
  --reshapeCallback $= Just reshape --callback a executar quando a janela for resized
  everythingRef <- newIORef =<< stat0
  enemyRef <- newIORef enemy0
  perWindowKeyRepeat $= PerWindowKeyRepeatOff	
  displayCallback $= mainDisplay everythingRef enemyRef
  mouseCallback $= Just (clickPos everythingRef enemyRef)

  mainLoop -- loop principal que controla os callbacks, a partir daqui o trabalho do openGL é automatico



mainDisplay :: IORef Everything -> IORef Enemy -> IO ()
mainDisplay everythingRef enemyRef = do
 everything <- readIORef everythingRef
 case (gameMode everything) of 
   Menu -> do mouseCallback $= Just (clickPos everythingRef enemyRef)
              displayMenu everythingRef
   SinglePlayer -> displaySingle everythingRef
   MultiPlayer -> displayMulti everythingRef enemyRef
   GameOver -> displayGameOver 
   
   
displayGameOver = do
 clear [ColorBuffer]
 renderPrimitive Quads $ do
  color3f 0 0 0
  vertex2 (-(widthf / 2)) (heigthf / 2)
  vertex2 (widthf / 2) (heigthf / 2)
  vertex2 (widthf / 2) (-(heigthf / 2)) 
  vertex2 (-(widthf / 2)) (-(heigthf / 2))  
 color3f 255 255 255
 renderText (-100,0) "Game over. Press space to return."
 flush


