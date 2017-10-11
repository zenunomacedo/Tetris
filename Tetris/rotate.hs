module Rotate where
import Data.List (nub)
import Data.IORef
import Types


cantRotate :: [(Int, Int)] -> Grelha -> Bool
cantRotate [] _ = False
cantRotate ((x,y):xs) g = ( (y < 20) && ((g!!(y))!!(x-1) /= Empty)) || cantRotate xs g  

notInBoundary :: [(Int, Int)] -> Bool
notInBoundary [] = False 
notInBoundary ((x,y):xs) = (x > 10) || (x < 1) || (y < 0) || notInBoundary xs

rotatePiece :: IORef Everything -> Grelha -> IO ()
rotatePiece everythingRef grelhaa = do
 everything <- readIORef everythingRef
 let (t, c, l@[(x1,y1), (x2,y2), (x3,y3), (x4,y4)]) = falling everything 
 let newL = rotatePoints t l
 if (notInBoundary newL) || (cantRotate newL grelhaa) then return () else modifyIORef everythingRef (\x -> x{falling = (t,c,newL)})

rotatePoints :: Char -> [(Int, Int)] -> [(Int, Int)]
rotatePoints t  l@[(x1,y1), (x2,y2), (x3,y3), (x4,y4)] = case t of
  'S' -> l
  
  
  '|' -> if 1 == length ( nub ( map snd l)) then [(x2, y2-1), (x2,y2), (x2,y2+1), (x2,y2+2)]
                                            else [(x2-1, y2), (x2,y2), (x2+1,y2), (x2+2,y2)]
                                          
                                          
  'L' -> if 2 == length ( nub ( map snd l)) then  --deitada
                
                if y1 < y2  then [(x3-1, y3+1), (x3, y3+1), (x3, y3), (x3, y3-1)] --2 -> 3
                            else [(x3+1,y3-1), (x3,y3-1), (x3, y3), (x3, y3+1)] --4->1?
                
                else --de pe
                
                if x1 > x2 then [(x3-1, y3-1), (x3-1, y3), (x3,y3), (x3+1, y3)]  -- 1 -> 2
                           else [(x3+1,y3+1), (x3+1, y3), (x3, y3), (x3-1, y3)] --3 -> 4


{-
x        xx                               x  x   xx                                          
x   xxx   x    x                          x  xxx x  xxx                                                              
xx  x     x  xxx                         xx      x    x                                                            
-}                           
  'l' -> if 2 == length ( nub ( map snd l)) then  --deitada
                
                if y1 > y2  then [(x3+1, y3+1), (x3, y3+1), (x3, y3), (x3, y3-1)] --2 -> 3
                            else [(x3-1,y3-1), (x3,y3-1), (x3, y3), (x3, y3+1)] --4->1?
                
                else --de pe
                
                if x1 < x2 then [(x3-1,y3+1), (x3+1, y3), (x3, y3), (x3-1, y3)]  -- 1 -> 2
                           else [(x3+1, y3-1), (x3-1, y3), (x3,y3), (x3+1, y3)] --3 -> 4
  
  
  'T' -> if 2 == length ( nub ( map snd l)) then --deitada
                if y1 < y2 then [(x3-1, y3), (x3, y3+1), (x3,y3), (x3, y3-1)]  --2 -> 3
                           else [(x3+1, y3), (x3, y3-1), (x3,y3), (x3, y3+1)]  --4 -> 1

         else 
                if x1 > x2 then [(x3, y3-1), (x3-1, y3), (x3,y3), (x3+1, y3)] --1 -> 2
                           else [(x3, y3+1), (x3+1, y3), (x3,y3), (x3-1, y3)]  --3 -> 4
                           
{-
  x       x  x                                               
  xx xxx xx xxx                                                     
  x   x   x                                                       
-}

  '4' -> if 3 == length ( nub ( map snd l)) then [(x1-2, y1), (x2-1, y2-1), (x3, y3), (x4+1, y4-1)] --1 -> 2 
                                            else [(x1+2, y1), (x2+1, y2+1), (x3, y3), (x4-1, y4+1)] --2 -> 1
  
{-
  x                                                      
  xx   xx                                                    
   x  xx                                                       
-}
  
  '8' -> if 3 == length ( nub ( map snd l)) then [(x1-1, y1+1), (x2, y2), (x3-1, y3-1), (x4, y4-2)] --1 -> 2 
                                            else [(x1+1, y1-1), (x2, y2), (x3+1, y3+1), (x4, y4+2)] --2 -> 1
  
{-
   x                                                      
  xx  xx                                                    
  x    xx                                                       
-}
  _ -> l 
rotatePoints _ _ = error "wtf"