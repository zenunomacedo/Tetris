module Types where
import Graphics.UI.GLUT (GLfloat)
import GHC.IO.Handle (Handle)
import System.Random
 
type Lado = GLfloat
type Centro = (GLfloat, GLfloat)
type Linha = [Piece]
type Grelha = [Linha]
type Falling = (Char, Colour, [(Int, Int)])
data Piece = Piece Colour | Empty deriving (Eq, Show, Read)
data Everything = Everything {grelha :: Grelha,
                              falling :: Falling,
                              next :: Falling,
                              getGhost :: [(Int, Int)], 
                              isFalling :: Bool,                               
                              interval :: Int, 
                              score :: Int, 
                              stored :: Falling, 
                              alreadyStored :: Bool,
                              gameMode :: GameMode
                              } deriving (Show)


data Enemy = Enemy { enemyGrelha :: Grelha,
                     enemyFalling :: Falling,
                     enemyScore :: Int, 
                     scrapLines :: [Linha],
                     getHandle :: Handle
                    }


stat0 :: IO Everything
stat0 = do
 piece0 <- newFalling
 piece1 <- newFalling
 return (Everything newGrelha piece0 piece1 [] False interval0 0 dummy False Menu)
  where dummy = ('F', Red, [])

enemy0 :: Enemy
enemy0 = Enemy newGrelha dummy 0 noScrap handle
 where dummy = ('F', Red, [])
       noScrap = []
       handle = undefined                    

interval0 :: Int
interval0 = 500 --tempo que demora uma peça a descer em ms       
newGrelha = replicate 20 $ replicate 10 Empty
emptyLine = replicate 10 Empty
testLine = [Piece Red, Empty, Piece Green, Empty, Piece Blue, Piece Blue, Empty, Piece Green, Empty, Piece Red]
testGrelha = testLine : (init newGrelha)

randomColour :: IO Colour
randomColour = do
 n <- randomRIO (1, length colours)
 return $ colours!!(n-1)

 
newFalling :: IO Falling
newFalling = do
 n <- randomRIO(1,7) :: IO Int
 c <- randomColour
 return $ matchPiece c ("SLlT|48"!!(n-1))
 
matchPiece :: Colour -> Char -> Falling
matchPiece c t = case t of
  'S' -> ('S', c, [(6,19), (5,19), (5,20), (6,20)])
  'L' -> ('L', c, [(6,19), (5,19), (5,20), (5,21)])
  'l' -> ('l', c, [(5,19), (6,19), (6,20), (6,21)])
  'T' -> ('T', c, [(6,20), (5,19), (5,20), (5,21)])
  '|' -> ('|', c, [(5,19), (5,20), (5,21), (5,22)])
  '4' -> ('4', c, [(6,19), (6,20), (5,20), (5,21)])
  '8' -> ('8', c, [(5,19), (5,20), (6,20), (6,21)])
  _ -> ('F', c, [])

  
data GameMode = Menu | SinglePlayer | MultiPlayer | GameOver deriving (Show, Eq)
data Colour = Red | Green | Blue | Orange | Purple | Yellow | Cyan | Pink | Gray deriving (Eq, Show, Read)
colours :: [Colour]
colours = [Red, Green, Blue, Orange, Purple, Yellow, Cyan, Pink]



getColour :: Colour -> (GLfloat, GLfloat, GLfloat)
getColour Red = (255,0,0)
getColour Green = (0, 204,0)
getColour Blue = (0,0,255)
getColour Orange = (255, 128, 0)
getColour Purple = (127, 0, 255)
getColour Yellow = (255, 255, 56)
getColour Cyan = (0,255,255)
getColour Pink = (255,0,127)
getColour Gray = (96,96,96)
