module SinglePlayer where
import Graphics.UI.GLUT hiding (Red, Blue, Green, renderPrimitive, Menu) --graficos
import Data.IORef --variaveis globais
import Control.Concurrent (threadDelay)
import System.Random hiding (next)
import Control.Monad (when)
import Rotate 
import Types

renderPrimitive = unsafeRenderPrimitive --RENDER PRIMITIVE MAS NAO PODE DAR EXCEPTION SENAO FUGGG :ddDDDD SUPOSTAMNETE É MAIS EFICAZ xDedos

heigth :: GLsizei
heigth = 600 --altura da janela
heigthf :: GLfloat
heigthf = fromIntegral heigth
width = 800 --largura da janela
widthf :: GLfloat
widthf = fromIntegral width
fps = 60 
timeWait = 16 --60 fps em ms
centro0 = ((-4.5) * (space), (-1)*( (fromIntegral heigth) / 2)+(space))
lado = 25
space = lado +2 --espaço entre peças

redisplay :: IORef Everything -> DisplayCallback                      
redisplay everythingRef = do                                            
 everything <- readIORef everythingRef                                  
 displaySingle everythingRef    
 threadDelay (if isFalling everything then timeWait * 500 else timeWait * 1000)
 if ((interval everything) - timeWait) < 1 
       then do                     
               update everythingRef
               everything2 <- readIORef everythingRef
               if (gameMode everything2 /= GameOver) then (writeIORef everythingRef (everything2{interval = (if isFalling everything2 then div interval0 10 else interval0)})) else gameOverScreen everythingRef
       else do
               everything2 <- readIORef everythingRef
               writeIORef everythingRef (everything2{interval = ((interval everything2) - timeWait) })
 when (gameMode everything /= GameOver) (addTimerCallback timeWait $ redisplay everythingRef)


 
displaySingle :: IORef Everything -> DisplayCallback
displaySingle everythingRef = do
  everything <- readIORef everythingRef
  clear [ColorBuffer]
  renderPrimitive Quads $ do  --desenhar baseado numa funçao de IO(), representada aqui pela 'do' notation, mas que irá ser desenvolvida para desenhar tudo no ecrã
    fundo 
    drawGrelha (grelha everything)
    drawFalling (falling everything)
    drawNext (next everything)
    drawStored ((uncurry matchPiece) $ (\(t,c,l) -> (c,t) ) $ stored everything)
    color3f 255 255 255
  renderText (-300,0) ((show $ score everything))
  renderText (-12 * space , 8 * space) "Next tile:" --(8s, 8s)
  renderText (-12 * space , space) "Score:"
  renderText (-12 * space , -4 * space) "Tile stored:"
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
    
    --vert for stored piece
    vertex2 (-8 * space) (-5 * space)
    vertex2 (-8 * space) (-9 * space)
    vertex2 (-12 * space) (-5 * space)
    vertex2 (-12 * space) (-9 * space)
    --horiz for stored piece
    vertex2 (-8 * space) (-5 * space)
    vertex2 (-12 * space) (-5 * space)
    vertex2 (-8 * space) (-9 * space)
    vertex2 (-12 * space) (-9 * space)
    
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

renderText :: (GLfloat, GLfloat) -> String -> IO ()
renderText (x,y) t = currentRasterPosition $= Vertex4 (2 * x / widthf) (2 * y / heigthf) 0 1 >> renderString Helvetica18 t
        
-- |Desenha uma linha de quadrados
drawLine :: Centro -> Int -> Linha -> IO ()
drawLine _ 0 _ = return ()
drawLine (x,y) n (Empty:xs) = drawLine (x+space, y) (n-1) xs
drawLine (x,y) n ((Piece c):xs) = do
 square lado (x,y) c
 drawLine (x+space, y) (n-1) xs

drawGrelha :: Grelha -> IO ()
drawGrelha = aux centro0 20
 where aux _ 0 _ = return ()
       aux (x,y) n (l:xs) = do
        drawLine (x,y) 10 l
        aux (x, y+space) (n-1) xs
 
drawFalling :: Falling -> IO ()
drawFalling (t, c, []) = return ()
drawFalling (t, c, ((x,y):xs)) = do
 when (y < 20) (square lado (fromIntegral ((x-1)) * (space) + x0, (fromIntegral (y)) * (space) + y0) c)
 drawFalling (t,c,xs)
  where (x0,y0) = centro0

                  
drawNext :: Falling -> IO ()
drawNext (t,c,[]) = return ()
drawNext (t,c,((x,y):xs)) = do
 square lado (-(a+10*space+xoff), b-5*space+yoff) c
 drawNext (t,c,xs)
    where (a,b) = (centro (x,y))
          xoff = if t == '|' then 0.5 * space else 0
          yoff = case t of
                  '|' -> (-0.4) * space
                  'S' -> 0.5 * space
                  _ -> 0

drawStored :: Falling -> IO ()
drawStored (t,c,[]) = return ()
drawStored (t,c,((x,y):xs)) = do
 square lado (a-10*space+xoff, b-17*space+yoff) c
 drawStored (t,c,xs)
    where (a,b) = (centro (x,y))
          xoff = if t == '|' then 0.5 * space else 0
          yoff = case t of
                  '|' -> (-0.4) * space
                  'S' -> 0.5 * space
                  _ -> 0
 
drawGhost :: Falling -> Grelha -> IORef Everything -> IO ()
drawGhost f@(t, c, l) grelhaa everythingRef = do
    if (collision l grelhaa) then do
                                    ghostPiece l
                                    modifyIORef everythingRef (\x -> x{getGhost = l})
                            else drawGhost (pull1down f) grelhaa everythingRef

ghostPiece :: [(Int, Int)] -> IO ()
ghostPiece [] = return ()
ghostPiece ((x,y):xs) = do
    when (y < 20) (ghost lado (centro (x,y)))
    ghostPiece xs
                            
ghost :: Lado -> Centro -> IO ()
ghost l (w,h) = do
        color3f 255 0 0
        vertex2 (w - l/2) (h + l/2)
        vertex2 (w + l/2) (h + l/2)
        vertex2 (w + l/2) (h + l/2)
        vertex2 (w + l/2) (h - l/2)
        vertex2 (w + l/2) (h - l/2)
        vertex2 (w - l/2) (h - l/2)
        vertex2 (w - l/2) (h - l/2)
        vertex2 (w - l/2) (h + l/2)
 
centro :: (Int, Int) -> Centro
centro (x, y) = ( x0 + (fromIntegral (x-1)) * space, y0 + (fromIntegral y) * space)
    where (x0, y0) = centro0 -- (GLfloat, GLfloat)

dropIt :: IORef Everything -> KeyboardCallback
dropIt everythingRef char _ = do
    everything <- readIORef everythingRef
    let (t,c,l) = falling everything
    let g = getGhost everything
    case char of
        ' ' ->  if (gameMode everything /= GameOver) 
                then writeIORef everythingRef ( everything{falling = (t,c,g), interval = 0} )
                else do
                     new <- stat0
                     writeIORef everythingRef new
                     keyboardCallback $= Nothing
        _ -> return ()
        
        



square :: Lado -> Centro -> Colour -> IO()
square l (w,h) c = do
        n <- randomRIO(0, (length colours) -1)
        let --crandom = colours !! n 
            (r,g,b) = getColour c
        color3f r g b
        vertex2 (w - l/2) (h + l/2)
        vertex2 (w + l/2) (h + l/2)    
        vertex2 (w + l/2) (h - l/2)
        vertex2 (w - l/2) (h - l/2)
       
color3f r g b = color $ Color3 ((r/255):: GLfloat) ((g/255):: GLfloat) ((b/255) :: GLfloat) --funçao para cores
--color3f (red) (green) (blue) em que estes valores sao [0..255]

vertex2 x y = vertex $ Vertex2 (((2 * x) /(realToFrac width))) (((2 * y)/(realToFrac heigth))::GLfloat)
--vertex2 x y desenha um vertice nas coordenadas (x,y) sendo o ponto (0,0) o centro do referencial. O 1º quadrante vai de (0,0) a (width / 2, heigth / 2)

releaseKey :: IORef Everything -> SpecialCallback
releaseKey everythingRef KeyDown _ = modifyIORef everythingRef (\x -> x{isFalling = False, interval = interval0})
releaseKey _ _ _ = return ()

move :: IORef Everything -> SpecialCallback
move everythingRef specialkey  _ = do
    everything <- readIORef everythingRef
    let (t, c, l) = falling everything
    case specialkey of
        KeyShiftL -> store (t,c,l) everythingRef
        KeyUp -> rotatePiece everythingRef (grelha everything)
        KeyDown -> writeIORef everythingRef ( everything{isFalling = True, interval = 0} )
        KeyLeft -> if (all (\(x,y) -> x > 1) l) && not (sideCol (-1) l (grelha everything)) then writeIORef everythingRef everything{falling = (t, c, map (\(x, y) -> (x-1, y)) l)} else return ()
        KeyRight -> if all (\(x,y) -> x < 10) l && not (sideCol (1) l (grelha everything)) then writeIORef everythingRef everything{falling = (t, c, map (\(x, y) -> (x+1, y)) l)} else return ()
        _ -> return()


               
update :: IORef Everything -> IO ()
update everythingRef = do
 everything <- readIORef everythingRef
 let (t,c,lP) = falling everything
 let g = grelha everything
 if collision lP g then do
   add lP c g everythingRef
   everything2 <- readIORef everythingRef
   when (gameMode everything2 /= GameOver) $ do
     checkLines everythingRef
     new <- newFalling
     modifyIORef everythingRef (\k ->  k{falling = next everything})
     modifyIORef everythingRef (\k ->  k{next = new, alreadyStored = False})
 else modifyIORef everythingRef (\k -> k{falling = pull1down (t,c,lP)})

reshape :: ReshapeCallback
reshape size = do
  --viewport $= (Position 0 0, size) --desloca as cenas na janela
  windowSize $= Size width heigth --no resizing 

pull1down :: Falling -> Falling
pull1down (t, c, lp) = (t, c, map (\(x,y) -> (x, pred y)) lp)
 
collision :: [(Int, Int)] -> Grelha -> Bool
collision [] _ = False
collision ((x,y):xs) g = if (y == 0) then True else if (y > 20) then False else (g!!(y-1))!!(x-1) /= Empty || collision xs g  -- x comeca em 1, y comeca em 0 (grelha)
 
sideCol :: Int -> [(Int, Int)] -> Grelha -> Bool
sideCol _ [] _ = False
sideCol n ((x,y):xs) g = ( if y > 19 then False else (g!!(y))!!(x-1+n) /= Empty) || sideCol n xs g  

store :: Falling -> IORef Everything -> IO()
store f everythingRef = do
 everything <- readIORef everythingRef
 let (tS, c, _) = stored everything
 if alreadyStored everything 
   then return () 
   else
    if tS == 'F' 
      then do
       new <- newFalling
       writeIORef everythingRef everything{stored = f, falling = next everything, next = new, alreadyStored = True}       
      else writeIORef everythingRef everything{stored = f, falling = matchPiece c tS, alreadyStored = True}

checkLines :: IORef Everything -> IO() --limpa as linhas
checkLines c = do
 d <- readIORef c
 aux c (grelha d) [] 0
 where aux  grelhaRef [] acc n = modifyIORef grelhaRef (\x -> x{grelha = (acc ++ (replicate n emptyLine)), score = score x + (if n == 4 then 5 else n)})
       aux grelhaRef (l:ls) acc n = if length (filter (/= Empty) l) == 10
                                    then aux grelhaRef ls acc (n+1)
                                    else aux grelhaRef ls (acc ++ [l]) n


add :: [(Int, Int)] -> Colour -> Grelha -> IORef Everything -> IO()
add [] c g eRef = modifyIORef' eRef (\x -> x{grelha = g})
add (pos@(x,y):xs) c g eRef = if y > 19 then modifyIORef' eRef (\k -> k{gameMode = GameOver}) else add xs c (aux pos g) eRef
 where aux (x,0) (h:t) = (aux' x h) : t
       aux (x,y) (h:t) = h : (aux (x, y-1) t)
       aux' 1 (x:xs) = (Piece c) : xs
       aux' n (x:xs) = x : (aux' (n-1) xs)

gameOverScreen :: IORef Everything -> IO ()
gameOverScreen everythingRef = do
 mouseCallback $= Nothing
 
       

