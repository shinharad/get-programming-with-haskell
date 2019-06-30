module Main where

import qualified Data.Map as Map

import           Lib

main :: IO ()
main = someFunc

data RobotPart =
  RobotPart
    { name        :: String
    , description :: String
    , cost        :: Double
    , count       :: Int
    }
  deriving (Show)

leftArm :: RobotPart
leftArm = RobotPart {name = "left arm", description = "left arm for face punching!", cost = 1000.00, count = 3}

rightArm :: RobotPart
rightArm = RobotPart {name = "right arm", description = "right arm for kind hand gestures", cost = 1025.00, count = 5}

robotHead :: RobotPart
robotHead = RobotPart {name = "robot head", description = "this head looks mad", cost = 5092.25, count = 2}

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>"
    , partName
    , "</h2>"
    , "<p><h3>desc</h3>"
    , partDesc
    , "</p><p><h3>cost</h3>"
    , partCost
    , "</p><p><h3>count</h3>"
    , partCount
    , "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

--insertSnippet :: Maybe Html -> IO ()

-- partVal: a Maybe RobotPart value
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- Using <$> to transform RobotPart to HTML, remaining in Maybe
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- A list of RobotParts
allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB
--allParts = map snd (Map.toList partsDB)

-- Transforming a list of RobotParts to HTML with <$> instead of map
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
--allPartsHtml = fmap renderHtml allParts

-- Functor for Map is concerned only about the Map’s values and not its keys.
-- When Map is made an instance of Functor, you’re concerned only about a single type variable,
-- the one used for its values. So for the purposes of Map being a member of Functor,
-- you treat it as being of kind * -> *.

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO


