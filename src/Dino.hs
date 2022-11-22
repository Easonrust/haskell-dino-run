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
  ,bushPos
  ,gameProgress
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
import System.Random (Random (..), getStdRandom, newStdGen)

-- Types

data Game = Game
  { _dino  :: Dino  -- ^ snake as a sequence of points in N2
  , _dir    :: Direction    -- ^ direction
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool   
  , _bushPos :: Int
  , _wall :: Int
  , _randP :: Int
  , _randPs :: Stream Int
  , _state :: Int	        -- ^ 0:startPage, 1:playPage, 2:gameOverPage
  , _history :: [Int]
  } deriving (Show)

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
    
-- | Step forward in time
gameProgress :: Game -> Game
gameProgress
  g@Game
    { _dead = l,
      _score = s,
      -- the rest
      _dino = a,
      _dir = d,
      _paused = p,
      _locked = m,
      _randP = rp,
      _randPs = rps,
      _bushPos = bushPos,
      _wall = w,
      _state = state,
      _history = history
    } =
        move
          Game
            { _dead = l,
              _score = s + 5,
              -- the rest
              _dino = a,
              _dir = d,
              _paused = p,
              _locked = m,
              _randP = rp,
              _randPs = rps,
              _bushPos = bushPos,
              _wall = w,
              _state = state,
      		  _history = history
            }


step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  MaybeT $ guard . not <$> orM [use paused, use dead]

  MaybeT . fmap Just $ locked .= False
  generateBush <|> MaybeT (Just <$> modify move)

generateBush :: MaybeT (State Game) ()
generateBush = do
  MaybeT . fmap guard $ (==) <$> (distanceToWall <$> get) <*> use wall
  MaybeT . fmap Just $ do
    get >>= \g -> modifying bushPos (nextBushPos g)
    nextRandomBush

nextRandomBush :: State Game ()
nextRandomBush =
  do
    (randp :| randps) <- use randPs
    randPs .= randps
    g <- get
    let touchWall = _bushPos g == 0
    if touchWall
      then nextRandomBush
      else randP .= randp


--move bush to the left by decreasing the x-coord
nextBushPos :: Game -> Int -> Int
nextBushPos g x = (x -1) `mod` width

distanceToWall :: Game -> Int
distanceToWall Game {_bushPos = bushPoss} = minimum [x | x <- [bushPoss]]

-- TODO collision and die



-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game {_dino = (s :|> _), _bushPos = bushPoss, _dead = l, _score = sc} =
  g & bushPos .~ ((bushPoss -1) `mod` width)

move _ = error "Dino can't be empty!"


lowboard :: Game -> Coord
lowboard Game { _dir = d, _dino = (a :<| _) } 
  | d == North = a & _y %~ (\y -> height) 
lowboard _ = error "Dino can't be empty!"

dinoJump :: Game -> Coord
dinoJump Game { _dir = d, _dino = (a :<| _) } = a & _y %~ (\y -> (y + 3) )
-- TODO: gravity



-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g@Game { _dino = (s :|> _) } = g & dino .~ (dinoJump g <| s)

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
        , _bushPos = width - 1
        , _wall = 0
        , _state = 1
        , _history = []
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
