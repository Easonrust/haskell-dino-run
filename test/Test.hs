{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import qualified Dino     as D
import qualified Data.Map as M 

main :: IO ()
main = runTests 
  [  probDino
  ]


probDino :: Score -> TestTree
probDino sc = testGroup "DinoRUNNN"
  [ scoreProp sc ("prop_genGameScore", D.prop_genGameScore, 1) 
  , scoreProp sc ("prop_genGameInterval", D.prop_genGameInterval, 2)
  , scoreProp sc ("prop_genGameBushGenerate", D.prop_genGameBushGenerate, 3)
  , scoreProp sc ("prop_genGameFruitGenerate", D.prop_genGameFruitGenerate, 4)
  , scoreProp sc ("prop_genGameMove", D.prop_genGameMove, 5)
  ]
