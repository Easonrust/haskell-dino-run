{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Dino
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead,  score, dino
  , height, width
  -- from C branch
  ,bushX
  ) where
import System.IO
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.List
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import System.Random (Random (..), getStdRandom, newStdGen)
import qualified Brick.Widgets.Dialog as D

-- Types

data Game = Game
  { _dino  :: Dino  -- ^ snake as a sequence of points in N2
  , _dir    :: Direction    -- ^ direction
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool   
  , _bushX :: Int
  , _wall :: Int
  , _randP :: Int
  , _randPs :: Stream Int
  , _state :: Int	        -- ^ 0:startPage, 1:playPage, 2:gameOverPage
  , _history :: [Int]
  , _startPageChoices :: D.Dialog Int
  } -- deriving (Show)

type Coord = V2 Int

type Dino = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width, gapSize, offset :: Int
height = 30
width = 30
gapSize = height * 3 `div` 10
offset = height `div` 6

-- Functions
split :: String -> [String] 
split [] = [""] 
split (c:cs) 
    | c == '\n' = "" : rest 
    | otherwise = (c : head rest) : tail rest 
    where rest = split cs
    

step :: Game -> Game
step s = flip execState s . runMaybeT $ do

  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use dead]

  -- Unlock from last directional turn
--   MaybeT . fmap Just $ locked .= False

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  die <|> MaybeT (Just <$> modify step')

die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ (isDie <$> get)
  MaybeT . fmap Just $ dead .= True

isDie :: Game -> Bool
isDie g@Game {_dino = ((V2 xm ym) :<| _), _bushX = bushXs}   
  | collide xm ym bushXs 2   = True
isDie _                      = False

collide :: Int -> Int -> Int -> Int -> Bool
collide dx dy bx by = dx == (bx-1) && (dy `elem` [0 .. by])

step':: Game -> Game
step' = move . increaseScore

generateBush :: MaybeT (State Game) ()
generateBush = do
  MaybeT . fmap guard $ (==) <$> (distanceToWall <$> get) <*> use wall
  MaybeT . fmap Just $ do
    get >>= \g -> modifying bushX (nextBushPos g)
    nextRandomBush

nextRandomBush :: State Game ()
nextRandomBush =
  do
    (randp :| randps) <- use randPs
    randPs .= randps
    g <- get
    let touchWall = _bushX g == 0
    if touchWall
      then nextRandomBush
      else randP .= randp


--move bush to the left by decreasing the x-coord
nextBushPos :: Game -> Int -> Int
nextBushPos g x = (x -1) `mod` width

distanceToWall :: Game -> Int
distanceToWall Game {_bushX = bushXs} = minimum [x | x <- [bushXs]]

-- TODO collision and die



-- | Move dino along in a marquee fashion
move :: Game -> Game
move g@Game {_dino = (s :|> _), _bushX = bushXs} =
  (gravity g) & bushX .~ ((bushXs -1) `mod` width)

move _ = error "Dino can't be empty!"

-- Move dino by y on the y axis
moveDino :: Int -> Game -> Game
moveDino y game = game & dino %~ fmap(+ V2 0 y)

getDinoY:: Game -> Int
getDinoY g@Game{_dino = (s :|> x)} = x ^. _y

gravity :: Game -> Game
gravity g = case (getDinoY g) of
              0 -> g
              _ -> (moveDino (-1) g)

lowboard :: Game -> Coord
lowboard Game { _dir = d, _dino = (a :<| _) } 
  | d == North = a & _y %~ (\y -> height) 
lowboard _ = error "Dino can't be empty!"

-- dinoJump :: Game -> Coord
-- dinoJump Game { _dir = d, _dino = (a :<| _) } = a & _y %~ (\y -> (y + 3) )
-- TODO: gravity
dinoJump :: Game -> Game
dinoJump g = if (getDinoY g == 0) then 
              moveDino 6 g
              else g

increaseScore :: Game -> Game
increaseScore g = g & score %~ (+5)


-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
-- turn d g@Game { _dino = (s :|> _) } = g & dino .~ (dinoJump g <| s)
turn d g = dinoJump g

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

initGame ::  IO Game
initGame = do
  (randp :| randps) <-
    fromList . randomRs (0 + offset, (height `div` 3) + offset) <$> newStdGen
  -- hard code initial bush length
  a <- drawInt (0 + offset) ((height `div` 3) + offset)
  b <- drawInt (0 + offset) ((height `div` 3) + offset)
  c <- drawInt (0 + offset) ((height `div` 3) + offset)
  let xm = 0
      ym = 0
      g  = Game
        { _dino  = (S.singleton (V2 xm ym))
        , _score  = 0
        , _dir    = South
        , _dead   = False
        , _paused = True
        , _locked = False
        , _randP = randp
        , _randPs = randps
        , _bushX = width - 1
        , _wall = 0
        , _state = 0
        , _history = []
        , _startPageChoices = D.dialog (Just "Dino Run!!!") (Just (0, [ ("Start", 0),("Quit", 1) ])) 50
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
