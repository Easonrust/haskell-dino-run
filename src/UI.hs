{-# LANGUAGE OverloadedStrings #-}
module UI where
import System.IO
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Data.List
-- import Data.List.Split
import Dino
import qualified Brick.Main as M
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Bush | Dino | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

split :: String -> [String] 
split [] = [""] 
split (c:cs) 
    | c == '\n' = "" : rest 
    | otherwise = (c : head rest) : tail rest 
    where rest = split cs


main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 400000 -- decides how fast your game moves
  g <- initGame 
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g


handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ gameProgress (step g)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing
drawGame :: Game -> [Widget Name]
drawGame g@Game{_dead=d} = if d then [ C.center $ padRight (Pad 2) (drawStats g)] else drawUI g

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGridSingle g ]

drawStats :: Game -> Widget Name
drawStats g@Game{_dead = d} = 
  if d==False 
  then 
      hLimit 11 $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ emptyWidget
         ]
  else
    drawGameOver g

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Game ->  Widget Name
drawGameOver g  =  vBox $ str "   Game Over" :
                             str " Your Score is" :
                             (str <$>  ["\t" <>  (show i) | i <- [g ^.score] ])

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Dino Run")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. dino = Dino
      | isBush g c         = Bush
      | otherwise          = Empty

drawGridSingle :: Game -> Widget Name
drawGridSingle g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Dino Run")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. dino = Dino
      | isBush g c         = Bush
      | otherwise          = Empty

isBush :: Game -> V2 Int -> Bool
isBush g (V2 x y)
  | x == g ^. bushPos && (y `elem` [0 .. 5]) = True
  | otherwise = False

gapSize :: Int
gapSize = height * 3 `div` 10

drawCell :: Cell -> Widget Name
drawCell Bush  = withAttr bushAttr cw
drawCell Dino  = withAttr dinoAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (bushAttr, V.blue `on` V.blue)
  , (dinoAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bushAttr, dinoAttr, emptyAttr :: AttrName
bushAttr = "bushAttr"
dinoAttr = "dinoAttr"
emptyAttr = "emptyAttr"
