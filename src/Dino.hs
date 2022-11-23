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
import System.IO.Unsafe
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
  , _dino_state :: DinoState
  , _velocity :: Int
  , _interval :: Int
  , _interval_len :: Int
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

data DinoState = LeftDino | RightDino | JumpDino | DeadDino

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

height, width, gapSize, offset, initVelocity :: Int
height = 30
width = 30
gapSize = height * 3 `div` 10
offset = height `div` 6
initVelocity = 5

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
move g@Game {_dino = (s :|> _), _bushPos = bushPoss, _dead = l, _score = sc, _interval = intr, _interval_len = len} =
  if (intr == 0) then
  (gravity g) & bushPos .~ ((bushPoss -1) `mod` width) & interval .~ (len) & velocity %~ (lossVelocity 1)
  else g & bushPos .~ ((bushPoss -1) `mod` width)

move _ = error "Dino can't be empty!"

lossVelocity :: Int -> Int -> Int
lossVelocity a v = if (v - a <= -initVelocity) then 
                    -initVelocity
                    else  v - a;

-- Move dino by y on the y axis
moveDino :: Int -> Game -> Game
moveDino y game = if (getDinoY game + y <= 0) then 
                  game & dino %~ fmap(+ V2 0 (-(getDinoY game)))
                  else game & dino %~ fmap(+ V2 0 y)

getDinoY:: Game -> Int
getDinoY g@Game{_dino = (s :|> x)} = x ^. _y

gravity :: Game -> Game
gravity g@Game {_velocity = v, _dino_state = state} = case (getDinoY g) of
              0 -> (case (state) of
                    LeftDino -> g & dino .~ (rightDino) & dino_state .~ (RightDino)
                    RightDino -> g & dino .~ (leftDino) & dino_state .~ (LeftDino)
                    _ -> g & dino .~ (leftDino) & dino_state .~ (LeftDino))
              _ -> (moveDino v g)
                    

lowboard :: Game -> Coord
lowboard Game { _dir = d, _dino = (a :<| _) } 
  | d == North = a & _y %~ (\y -> height) 
lowboard _ = error "Dino can't be empty!"

-- dinoJump :: Game -> Coord
-- dinoJump Game { _dir = d, _dino = (a :<| _) } = a & _y %~ (\y -> (y + 3) )
-- TODO: gravity
dinoJump :: Game -> Game
dinoJump g@Game {_interval_len = len, _velocity = v} = if (getDinoY g == 0) then 
              moveDino initVelocity (g & velocity .~ (initVelocity - 1)  & interval .~ (len) & 
              dino .~ (jumpDino) & dino_state .~ (JumpDino))
              else g & velocity %~ (\_ -> 0) 

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
        { _dino  = leftDino
        , _dino_state = LeftDino
        , _velocity = 0
        , _interval = 1
        , _interval_len = 1
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


leftDino :: Dino
leftDino = makeDino "image/out_left.txt"

rightDino :: Dino
rightDino = makeDino "image/out_right.txt"

jumpDino :: Dino
jumpDino = makeDino "image/out_jump.txt"

diedDino :: Dino
diedDino = makeDino "image/out_dead.txt"

makeDino :: String -> Dino
makeDino file = unsafePerformIO (initDino' file)

initDino' :: String -> IO Dino
initDino' file =  openFile file ReadMode >>= \handle ->
    hGetContents handle >>= \contents -> 
    return (executeList (helperfnc (words contents)))
     

executeList :: [Int] -> Dino
executeList (x:y:xs) = (S.singleton (V2 y x)) S.>< (executeList xs);
executeList _ = S.empty

helperfnc :: [String] -> [Int]
helperfnc = map read
