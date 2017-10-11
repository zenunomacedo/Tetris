module Multiplayer where
import SinglePlayer
import Types
import Graphics.UI.GLUT hiding (Red, Blue, Green, renderPrimitive) --graficos
import Control.Monad (when, replicateM)
import Data.IORef
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import System.IO (hGetLine, hFlush, hPutStrLn)


redisplayMulti :: IORef Everything -> IORef Enemy-> DisplayCallback                      
redisplayMulti everythingRef enemyRef = do                                            
 everything <- readIORef everythingRef                                  
 addTimerCallback timeWait $ redisplayMulti everythingRef enemyRef
 sendInfo everythingRef enemyRef
 displayMulti everythingRef enemyRef 
 threadDelay (if isFalling everything then timeWait * 500 else timeWait * 1000)
 if ((interval everything) - timeWait) < 1 
       then do  
               update everythingRef
               everything2 <- readIORef everythingRef
               writeIORef everythingRef (everything2{interval = (if isFalling everything2 then div interval0 10 else interval0)})
       else do
               everything2 <- readIORef everythingRef
               writeIORef everythingRef (everything2{interval = ((interval everything2) - timeWait) })
  

sendInfo :: IORef Everything -> IORef Enemy -> IO ()
sendInfo everythingRef enemyRef = do
 everything <- readIORef everythingRef
 enemy <- readIORef enemyRef
 let handle = getHandle enemy
 hPutStrLn handle $ show (falling everything)
 hPutStrLn handle $ show $ compress (grelha everything)
 hPutStrLn handle $ show (score everything)
 hFlush handle
  
  
readInfo :: IORef Everything -> IORef Enemy -> IO()
readInfo everythingRef enemyRef = do
 everything <- readIORef everythingRef
 enemy <- readIORef enemyRef
 let handle = getHandle enemy
 inputFalling <- hGetLine handle
 inputGrelhaString <- hGetLine handle
 let inputGrelha = recover (read inputGrelhaString :: [String])
 newScore <- fmap read $ hGetLine handle
 when (newScore > enemyScore enemy) (do --Ganhou score
                                        scrap <- replicateM (newScore - (enemyScore enemy) - 1) randomLine
                                        modifyIORef' everythingRef (\x -> x{grelha = take 20 $ (scrap) ++ (grelha x)}) 
                                    )
 writeIORef enemyRef enemy{enemyFalling = read inputFalling :: Falling, enemyGrelha = inputGrelha, enemyScore = newScore}
 when (gameMode everything == MultiPlayer) $ readInfo everythingRef enemyRef
 
compress :: Grelha -> [String] 
compress = map (map convertOne)

convertOne :: Piece -> Char --pode-se preservar cores usando diferentes caracteres, mas por agora n e preciso
convertOne Empty = 'E'
convertOne _ = 'P'

recover :: [String] -> Grelha
recover = map (map recoverOne)

recoverOne :: Char -> Piece
recoverOne 'E' = Empty
recoverOne _ = Piece Red
 
 
displayMulti :: IORef Everything -> IORef Enemy -> IO () 
displayMulti everythingRef enemyRef = do
  everything <- readIORef everythingRef
  enemy <- readIORef enemyRef
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    fundo 
    drawGrelha (grelha everything)
    drawFalling (falling everything)
    drawNext (next everything)
    drawStored ((uncurry matchPiece) $ (\(t,c,l) -> (c,t) ) $ stored everything)
    drawGrelhaEnemy $ enemyGrelha enemy
    drawFallingEnemy $ enemyFalling enemy
    color3f 255 255 255
  renderText (-300,0) ((show $ score everything))
  renderText (-12 * space , 8 * space) "Next tile:"
  renderPrimitive Lines $ do
    color3f 255 255 255
    --vert for game grid
    vertex2 (-5 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (-5 * space) ((20 * space) + ((snd centro0) - (0.5 * lado)))
    vertex2 (5 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (5 * space) ((20 * space) + ((snd centro0) - (0.5 * lado)))
    --horiz for game grid
    vertex2 (-5 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (5 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (-5 * space) ((20 * space) + ((snd centro0) - (0.5 * lado)))
    vertex2 (5 * space) ((20 * space) + ((snd centro0) - (0.5 * lado)))
    
    --vert for enemy grid
    vertex2 (8 * space) ((snd centro0) - (0.5 * lado) ) --each with side 0.4space
    vertex2 (8 * space) ((20 * spaceE) + ((snd centro0) - (0.5 * lado)))
    vertex2 (12 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (12 * space) ((20 * spaceE) + ((snd centro0) - (0.5 * lado)))
    --horiz for enemy grid
    vertex2 (8 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (12 * space) ((snd centro0) - (0.5 * lado))
    vertex2 (8 * space) ((20 * spaceE) + ((snd centro0) - (0.5 * lado)))
    vertex2 (12 * space) ((20 * spaceE) + ((snd centro0) - (0.5 * lado)))
    
    --vert for next piece
    vertex2 (-8 * space) (7 * space)
    vertex2 (-8 * space) (3 * space)
    vertex2 (-12 * space) (7 * space)
    vertex2 (-12 * space) (3 * space)
    --horiz for next piece
    vertex2 (-8 * space) (7 * space)
    vertex2 (-12 * space) (7 * space)
    vertex2 (-8 * space) (3 * space)
    vertex2 (-12 * space) (3 * space)
    --ghosting the piece
    drawGhost (falling everything) (grelha everything) (everythingRef)
    
  flush --flush pushes our OpenGL commands down to the system graphics for actual displaySingle
  where 
       fundo = do
        color3f 0 0 0
        vertex2 (-(widthf / 2)) (heigthf / 2)
        vertex2 (widthf / 2) (heigthf / 2)
        vertex2 (widthf / 2) (-(heigthf / 2)) 
        vertex2 (-(widthf / 2)) (-(heigthf / 2))  

spaceE = space * 0.4
ladoE = lado * 0.4
centroE = (8.5 * space, y0) 
 where (_,y0) = centro0

randomLine :: IO Linha
randomLine = do
 x <- randomRIO (1,2) :: IO Int
 n <- randomRIO (1,10)
 case x of
  1 -> return $ (replicate (n-1) (Piece Gray)) ++ [Empty] ++ (replicate (10-n) (Piece Gray))
  2 -> return $ (replicate (n-2) (Piece Gray)) ++ [Empty, Empty] ++ (replicate (10-n) (Piece Gray)) 
 
drawLineEnemy :: Centro -> Int -> Linha -> IO ()
drawLineEnemy _ 0 _ = return ()
drawLineEnemy (x,y) n (Empty:xs) = drawLineEnemy (x+(spaceE), y) (n-1) xs
drawLineEnemy (x,y) n ((Piece c):xs) = do
 square (ladoE) (x,y) Red
 drawLineEnemy (x+spaceE, y) (n-1) xs

drawGrelhaEnemy :: Grelha -> IO ()
drawGrelhaEnemy = aux centroE 20
 where aux _ 0 _ = return ()
       aux (x,y) n (l:xs) = do
        drawLineEnemy (x-0.5*spaceE,y-0.5*spaceE) 10 l
        aux (x, y+(spaceE)) (n-1) xs
 
drawFallingEnemy :: Falling -> IO ()
drawFallingEnemy (t, c, []) = return ()
drawFallingEnemy (t, c, ((x,y):xs)) = do
 when (y < 20) (square (ladoE) (fromIntegral ((x-1)) * (spaceE) + xE -0.5 * spaceE, (fromIntegral (y)) * (spaceE) + yE) Cyan)
 drawFallingEnemy (t,c,xs)
  where (xE,yE) = centroE