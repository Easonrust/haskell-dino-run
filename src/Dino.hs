{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Dino
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , Bush,Fruit
  , Coord
  , dead, score, dino
  , height, width
  , bushes,fruits
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
import Data.Foldable (toList)
import Debug.Trace
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import System.Random (Random (..), getStdRandom, newStdGen)
import qualified Brick.Widgets.Dialog as D
import Prelude hiding ((!!))
import Data.Monoid
import Test.QuickCheck 

-- Types

data Game = Game
  { _dino  :: Dino  -- ^ snake as a sequence of points in N2
  , _dino_state :: DinoState
  , _bushes :: Seq Bush
  , _fruits :: Seq Fruit
  , _velocity :: Int
  , _interval :: Int
  , _interval_len :: Int
  , _dir    :: Direction    -- ^ direction
  , _dead   :: Bool         -- ^ game over flag
  , _ingame :: Bool
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool   
  , _wall :: Int
  , _randP :: Int
  , _randPs :: Stream Int
  , _lastBushPos :: Int
  , _lastFruitPos :: Int
  , _tDelay :: Int
  , _state :: Int	        -- ^ 0:startPage, 1:playPage, 2:gameOverPage, 3:difficultyPage
  , _difficulty :: Int
  , _max_score :: Int
  , _history :: [Int]
  , _startPageChoices :: D.Dialog Int
  , _diffPageChoices :: D.Dialog Int
  , _endPageChoices :: D.Dialog Int
  } -- deriving (Show)
 
instance (Show Game) where
	show g@Game{_score = s, _max_score = m} = show s ++ "-" ++ show m

type Coord = V2 Int

type Dino = Seq Coord
type Bush = Seq Coord
type Fruit = Seq Coord
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
width = 100
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
  MaybeT $ guard . not <$> use dead
  MaybeT $ guard <$> use ingame

  -- Unlock from last directional turn
--   MaybeT . fmap Just $ locked .= False

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  -- die <|> MaybeT (Just <$> modify step')
  die <|> MaybeT (Just <$> modify step')

die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ (isDie <$> get)
  MaybeT . fmap Just $ dead .= True
  MaybeT . fmap Just $ ingame .= False

-- TODO:
isDie :: Game -> Bool
isDie g@Game {_dino = dino, _bushes = bushes} = getAny $ foldMap (Any . flip collide bushes) dino

hitFruit :: Game -> Bool
hitFruit g@Game {_dino = dino, _fruits = fruits} = getAny $ foldMap (Any . flip collide fruits) dino

collide :: Coord -> Seq Bush -> Bool
collide dino bushes = getAny $ foldMap (Any . collide' dino) bushes

-- check to see if crashed in a specific obstacle
collide' :: Coord -> Bush -> Bool
collide' dino bush = dino `elem` bush

step':: Game -> Game
step' = move . increaseScore . decreaseInterval . generateBush . generateFruit . checkFruit

-- TODO: generate infinity bushes list
-- generateBush :: MaybeT (State Game) ()
-- generateBush = do
--   MaybeT . fmap guard $ (==) <$> (distanceToWall <$> get) <*> use wall
--   MaybeT . fmap Just $ do
--     get >>= \g -> modifying bushX (nextBushPos g)
--     nextRandomBush

-- nextRandomBush :: State Game ()
-- nextRandomBush =
--   do
--     (randp :| randps) <- use randPs
--     randPs .= randps
--     g <- get
--     let touchWall = _bushX g == 0
--     if touchWall
--       then nextRandomBush
--       else randP .= randp


--move bush to the left by decreasing the x-coord
-- nextBushPos :: Game -> Int -> Int
-- nextBushPos g x = (x -1) `mod` width

-- distanceToWall :: Game -> Int
-- distanceToWall Game {_bushX = bushXs} = minimum [x | x <- [bushXs]]

-- TODO collision and die



-- | Move dino along in a marquee fashion
move :: Game -> Game
move g@Game {_dino = (s :|> _), _dead = l, _score = sc, _interval = intr, _interval_len = len} =
  if (intr == 0) then
  (gravity g) & bushes %~ (fmap moveBush) & interval .~ (len) & velocity %~ (lossVelocity 1) & fruits %~ (fmap moveFruit)
--   else g & bushX .~ ((bushXs -1) `mod` width)
  else g & bushes %~ (fmap moveBush) & fruits %~ (fmap moveFruit)
move _ = error "Dino can't be empty!"

moveBush :: Bush -> Bush
moveBush bush  = fmap (+ V2 (-1) 0) bush 

moveFruit :: Fruit -> Fruit
moveFruit fruit  = fmap (+ V2 (-1) 0) fruit 

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

deleteFruit :: Game -> Game
deleteFruit g@Game{_fruits = (fruit :<| s)} = g & fruits .~ s

checkFruit :: Game -> Game
checkFruit g@Game{_score=s}= do 
                if hitFruit g
                  -- Add 100 pt, remove the fruit
                  then deleteFruit g {_score = s+100}
                  else removeOutLieFruit g
                
                

removeOutLieFruit :: Game -> Game
removeOutLieFruit g@Game{_fruits = ((co:<|fruit) :<| s)} = do
                                                if (co^. _x) <0
                                                  then  g & fruits .~ s
                                                  else  g
                                                


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
increaseScore g@Game {_score = s, _max_score = mx} = g {_score = s + 5, _max_score = (max mx (s+5))}

decreaseInterval :: Game -> Game
decreaseInterval g@Game {_interval = i} = g & interval .~ (i-1)

generateBush :: Game -> Game
generateBush g@Game {_lastBushPos = l, _difficulty=diff} = g & bushes %~ (S.|> (makeBush newlastBP)) & lastBushPos .~ newlastBP
    where newlastBP = unsafePerformIO (drawInt ((2-diff)*10+l+25) ((2-diff)*10+l+60))

generateFruit :: Game -> Game
generateFruit g@Game {_lastFruitPos = l} = g & fruits %~ (S.|> (makeFruit newlastBP)) & lastFruitPos .~ newlastBP
    where newlastBP = unsafePerformIO (drawInt (l+40) (l+60))


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
  a <- drawInt 40 50
  b <- drawInt 70 80
  c <- drawInt 100 120
  max_score_txt <- readFile "data/max_score.txt"
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
        , _ingame = False
        , _paused = True
        , _locked = False
        , _randP = randp
        , _randPs = randps
        , _bushes = S.fromList [makeBush a, makeBush b, makeBush c]
        , _lastBushPos = c
        , _fruits = S.fromList [makeFruit (a-4), makeFruit (b+4), makeFruit (c-4)]
        , _lastFruitPos = c-1
        , _wall = 0
        , _state = 0
        , _tDelay = 200000
        , _history = []
        , _difficulty = 0
        , _max_score = read max_score_txt :: Int
        , _diffPageChoices = D.dialog (Just "Select Mode") (Just (0, [ ("Easy", 0),("Medium", 1),("Hard", 2) ])) 50
        , _startPageChoices = D.dialog (Just "Dino Run!!!") (Just (0, [ ("Start", 0),("Quit", 1) ])) 50
        , _endPageChoices = D.dialog (Just "Game Over") (Just (0, [ ("Restart", 0),("Quit", 1) ])) 50
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

makeBush :: Int -> Bush
makeBush bushX = S.fromList [V2 bushX 0, V2 (bushX+1) 0, V2 bushX 1, V2 (bushX+1) 1, V2 bushX 2, V2 (bushX+1) 2, V2 bushX 3, V2 (bushX+1) 3, V2 bushX 4, V2 (bushX+1) 4, V2 bushX 5, V2 (bushX+1) 5, V2 bushX 6, V2 (bushX+1) 6]
     
makeFruit :: Int -> Fruit
makeFruit fruitX = S.fromList [V2 fruitX 16]


executeList :: [Int] -> Dino
executeList (x:y:xs) = (S.singleton (V2 (y `div` 2) (x `div` 2))) S.>< (executeList xs);
executeList _ = S.empty

helperfnc :: [String] -> [Int]
helperfnc = map read


---- properties/tests ----

-- increaseScore :: Game -> Game
-- increaseScore g@Game {_score = s, _max_score = mx} = g {_score = s + 5, _max_score = (max mx (s+5))}

genGame :: Gen Game
genGame = do
	s <- chooseInt (0, 1000)
	mx <- chooseInt (0, 1000)
	let g = Game {_score = s, _max_score = mx}
	return g

prop_genGame :: Property
prop_genGame = forAll genGame (\g -> (_score (increaseScore g)) == (_score g) + 5)



