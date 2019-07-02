module Main where

import qualified Data.Map as Map

import           Lib

main :: IO ()
main = someFunc

-- THE LIMITATIONS OF APPLICATIVE AND FUNCTOR
type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep")
    , (2, "KINGinYELLOW")
    , (3, "dagon1997")
    , (4, "rcarter1919")
    , (5, "xCTHULHUx")
    , (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000)
    , ("KINGinYELLOW", 15000)
    , ("dagon1997", 300)
    , ("rcarter1919", 12)
    , ("xCTHULHUx", 50000)
    , ("yogSOThoth", 150000)
    ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- altLookupCredits, a solution without using Functor or Applicative
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just username) = lookupCredits username

-- Going straight from GamerId -> Maybe PlayerCredits
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- MaybeはJust、Nothingしかないので何とかなるが...

-- 30.2. THE BIND OPERATOR: >>=

