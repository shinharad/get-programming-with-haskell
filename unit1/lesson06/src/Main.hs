module Main where

main :: IO ()
main = return ()

assignToGroups n aList = zip groups aList
  where
    groups = cycle [1 .. n]

main1 = do
  print $
    assignToGroups
      3
      ["file1.txt", "file2.txt", "file3.txt", "file4.txt", "file5.txt", "file6.txt", "file7.txt", "file8.txt"]
      -- [(1,"file1.txt"),(2,"file2.txt"),(3,"file3.txt"),(1,"file4.txt"),(2,"file5.txt"),(3,"file6.txt"),(1,"file7.txt"),(2,"file8.txt")]
  print $ assignToGroups 2 ["Bob", "Kathy", "Sue", "Joan", "Jim", "Mike"]
    -- [(1,"Bob"),(2,"Kathy"),(1,"Sue"),(2,"Joan"),(1,"Jim"),(2,"Mike")]
