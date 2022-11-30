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
import Data.Monoid

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
  g <- initGame
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 200000 -- decides how fast your game moves 
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g


handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g@Game{_state = 0} (VtyEvent ev)		= handleStartPage g ev
handleEvent g@Game{_dead = True} (VtyEvent (V.EvKey (V.KChar 's') []))      = liftIO (writeMaxScore g) >>= continue
handleEvent g@Game{_dead = True} (VtyEvent ev) 		= handleEndPage g ev --  >>= halt
handleEvent g@Game{_state = 3} (VtyEvent ev)		= handleDiffPage g ev
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
      Just 0  -> Brick.continue (g {_state = 3})
      Just 1  -> Brick.halt g
      Nothing -> Brick.continue (g {_state = 0})
  _ -> do
    dialog <- D.handleDialogEvent ev (_startPageChoices g)
    Brick.continue (g {_startPageChoices = dialog})
    
handleDiffPage :: Game -> V.Event -> EventM n (Next Game)
handleDiffPage g ev = case ev of
  V.EvKey V.KEsc []   -> Brick.continue (g {_state = 0})
  V.EvKey V.KEnter [] -> do
    dialog <- D.handleDialogEvent ev (_diffPageChoices g)
    case (D.dialogSelection dialog) of
      Just 0  -> Brick.continue (g {_state = 1, _difficulty = 0, _tDelay = 200000})
      Just 1  -> Brick.continue (g {_state = 1, _difficulty = 1, _tDelay = 150000})
      Just 2  -> Brick.continue (g {_state = 1, _difficulty = 2, _tDelay = 100000})
      Nothing -> Brick.continue (g {_state = 0})
  _ -> do
    dialog <- D.handleDialogEvent ev (_diffPageChoices g)
    Brick.continue (g {_diffPageChoices = dialog})
    
handleEndPage :: Game -> V.Event -> EventM n (Next Game)
handleEndPage g ev = case ev of
  V.EvKey V.KEsc []   -> Brick.halt g
  V.EvKey V.KEnter [] -> do
    dialog <- D.handleDialogEvent ev (_endPageChoices g)
    case (D.dialogSelection dialog) of
      Just 0  -> liftIO (initGame) >>= continue
      Just 1  -> Brick.halt g
      Nothing -> Brick.continue g
  _ -> do
    dialog <- D.handleDialogEvent ev (_endPageChoices g)
    Brick.continue (g {_endPageChoices = dialog})

writeMaxScore :: Game -> IO Game
writeMaxScore g@Game {_score = s} = do
  _ <- writeFile "data/max_score.txt" (show s)
  return g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g@Game{_state = 0} = drawStartPage g
drawUI g@Game{_state = 1} = drawPlayPage g
drawUI g@Game{_state = 2} = drawEndPage g
drawUI g@Game{_state = 3} = drawDiffPage g

drawDiffPage :: Game -> [Widget n]
drawDiffPage g = [ui]
  where
    ui = D.renderDialog (_diffPageChoices g) $ C.hCenter $ padAll 1 $ str "   "
  
drawStartPage :: Game -> [Widget n]
drawStartPage g = [ui]
  where
    ui = D.renderDialog (_startPageChoices g) $ C.hCenter $ padAll 1 $ str "   "

drawEndPage :: Game -> [Widget n]
drawEndPage g = [C.center $ padRight (Pad 2) (drawGameOver g)]

drawGameOver :: Game -> Widget n
drawGameOver g = D.renderDialog (_endPageChoices g) $ C.hCenter $ padAll 1 $ str (" Your Score is: " ++ show (g ^. score) ++ "\nPress s to save the score.")

drawPlayPage :: Game -> [Widget Name]
drawPlayPage g@Game{_dead=d} = if d then [ drawStats g ] else drawPlayUI g

drawPlayUI :: Game -> [Widget Name]
drawPlayUI g = [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGridSingle g ]

--drawStats :: Game -> Widget Name
--drawStats g = hLimit 30 
--(vBox [drawGameOver (g^.alive), 
--padTop (Pad 2) (drwaMode (g^.mode)), 
--padTop (Pad 2) (drawDepth (g^.depth)), 
--padTop (Pad 2) (drawBestDepth (g^.maxDepth))])


drawStats :: Game -> Widget Name
drawStats g@Game{_dead = d} = 
  if d==False 
  then 
      hLimit 11 $ vBox [ emptyWidget
      					,padTop (Pad 2) (drawScore (g ^. score))
      					,padTop (Pad 2) (drawMaxScore (_max_score g))
	      				,padTop (Pad 2) (drawMode (_difficulty g)) ]
  else
    drawGameOver g

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawMaxScore :: Int -> Widget Name
drawMaxScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Max Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n
  
drawMode :: Int -> Widget Name
drawMode n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Mode")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

-- drawGameOver :: Game ->  Widget Name
-- drawGameOver g  =  vBox $ str "   Game Over" :
--                             str " Your Score is" :
--                             (str <$>  ["\t" <>  (show i) | i <- [g ^.score] ])

-- drawGrid :: Game -> Widget Name
-- drawGrid g = withBorderStyle BS.unicodeBold
--   $ B.borderWithLabel (str "Dino Run")
--   $ vBox rows
--   where
--     rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
--     cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
--     drawCoord    = drawCell . cellAt
--     cellAt c
--       | c `elem` g ^. dino = Dino
--       | isBush g c         = Bush
--       | otherwise          = Empty

drawGridSingle :: Game -> Widget Name
drawGridSingle g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Dino Run")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. dino   = Dino
      | isBush c (g^.bushes) = Bush
      | otherwise            = Empty

isBush :: Coord -> Seq Bush -> Bool
isBush c bs = getAny $ foldMap (Any . isBush' c) bs

isBush' :: Coord -> Bush -> Bool
isBush' c b = c `elem` b
isBush' _ _  = False

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
