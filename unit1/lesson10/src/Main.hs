module Main where

main :: IO ()
main = return ()

cup flOz = \message -> message flOz

getOz aCup = aCup (\flOz -> flOz)

-- drink aCup ozDrank = cup (flOz - ozDrank)
--   where flOz = getOz aCup
-- drink aCup ozDrank = if ozDiff >= 0
--                      then cup ozDiff
--                      else cup 0
drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank
-- afterManySips coffeeCup = foldl drink coffeeCup [1,1,1,1]
