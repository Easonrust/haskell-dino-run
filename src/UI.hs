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
import qualified Brick.Widgets.Dialog as D
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
app = App { appDraw = drawUI
          , appChooseCursor = const . const Nothing
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
handleEvent g@Game{_state = 0} (VtyEvent ev)		= handleStartPage g ev
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g


-- page transfer
handleStartPage :: Game -> V.Event -> EventM n (Next Game)
handleStartPage g ev = case ev of
  V.EvKey V.KEsc []   -> Brick.halt g
  V.EvKey V.KEnter [] -> do
    dialog <- D.handleDialogEvent ev (_startPageChoices g)
    case (D.dialogSelection dialog) of
      Just 0  -> Brick.continue (g {_state = 1})
      Just 1  -> Brick.halt g
      Nothing -> Brick.continue (g {_state = 0})
  _ -> do
    dialog <- D.handleDialogEvent ev (_startPageChoices g)
    Brick.continue (g {_startPageChoices = dialog})
  
-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g@Game{_state = 0} = drawStartPage g
drawUI g@Game{_state = 1} = drawPlayPage g
drawUI g@Game{_state = 2} = drawEndPage g
  
drawStartPage :: Game -> [Widget n]
drawStartPage g = [ui]
  where
    ui = D.renderDialog (_startPageChoices g) $ C.hCenter $ padAll 1 $ str "   "

drawEndPage :: Game -> [Widget n]
drawEndPage g = [C.center $ padRight (Pad 2) (drawGameOver g)]

drawGameOver :: Game -> Widget n
drawGameOver g = 
  vBox $
    str "   Game Over" :
    str " Your Score is: " :
    (str <$>  ["\t" <>  (show i) | i <- [g ^.score] ])
    

drawPlayPage :: Game -> [Widget Name]
drawPlayPage g@Game{_dead=d} = if d then [ C.center $ padRight (Pad 2) (drawStats g)] else drawPlayUI g

drawPlayUI :: Game -> [Widget Name]
drawPlayUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGridSingle g ]

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

-- drawGameOver :: Game ->  Widget Name
-- drawGameOver g  =  vBox $ str "   Game Over" :
--                             str " Your Score is" :
--                             (str <$>  ["\t" <>  (show i) | i <- [g ^.score] ])

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
  | x == g ^. bushX && (y `elem` [0 .. 2]) = True
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
  , (D.dialogAttr, V.white `on` V.blue)
  , (D.buttonAttr, V.black `on` V.white)
  , (D.buttonSelectedAttr, V.yellow `on` V.white)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bushAttr, dinoAttr, emptyAttr :: AttrName
bushAttr = "bushAttr"
dinoAttr = "dinoAttr"
emptyAttr = "emptyAttr"
