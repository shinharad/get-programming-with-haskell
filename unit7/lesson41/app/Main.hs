module Main where

import           Control.Monad
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Tool =
  Tool
    { toolId        :: Int
    , name          :: String
    , description   :: String
    , lastReturned  :: Day
    , timesBorrowed :: Int
    }

data User =
  User
    { userId   :: Int
    , userName :: String
    }

instance Show User where
  show user = mconcat [show $ userId user, ".)  ", userName user]

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool
      , ".) "
      , name tool
      , "\n description: "
      , description tool
      , "\n last returned: "
      , show $ lastReturned tool
      , "\n times borrowed: "
      , show $ timesBorrowed tool
      , "\n"
      ]

-- Adding new users to your database
addUser :: String -> IO ()
addUser userName = do
  conn <- open "tools.db"
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
  close conn

--    print "user added"
-- Creating checkouts
checkout :: Int -> Int -> IO ()
checkout userId toolId =
  withConn "tools.db" $ \conn -> do
    execute conn "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)" (userId, toolId)

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

-- Making your data an instance of FromRow
instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

-- Printing users from your database
printUsers :: IO ()
printUsers =
  withConn "tools.db" $ \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

-- A generic way to run any queries of tools from your database
printToolQuery :: Query -> IO ()
printToolQuery q =
  withConn "tools.db" $ \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable =
  printToolQuery $ mconcat ["select * from tools ", "where id not in ", "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $ mconcat ["select * from tools ", "where id in ", "(select tool_id from checkedout);"]

-- UPDATING EXISTING DATA
-- Safely selecting a Tool by ID
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing []    = Nothing
firstOrNothing (x:_) = Just x

-- updateTool updates your tool
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool {lastReturned = date, timesBorrowed = 1 + timesBorrowed tool}

-- Safely updating your database
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) =
  withConn "tools.db" $ \conn -> do
    let q = mconcat ["UPDATE TOOLS SET  ", "lastReturned = ?,", " timesBorrowed = ? ", "WHERE ID = ?;"]
    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
    print "tool updated"

-- updateToolTable combines all the steps for updating the tool table
updateToolTable :: Int -> IO ()
updateToolTable toolId =
  withConn "tools.db" $ \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

--instance ToRow Tool where
--  toRow tool =
--    [ SQLInteger $ fromIntegral $ toolId tool
--    , SQLText $ T.pack $ name tool
--    , SQLText $ T.pack $ description tool
--    , SQLText $ T.pack $ show $ lastReturned tool
--    , SQLInteger $ fromIntegral $ timesBorrowed tool
--    ]
-- DELETING DATA FROM YOUR DATABASE
-- Checking in a tool with checkin
checkin :: Int -> IO ()
checkin toolId =
  withConn "tools.db" $ \conn -> do execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

-- Making sure your tool is updated when it’s checked in
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

-- Organizing your database actions
promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- pure read <*> getLine
  print "Enter the id of the tool"
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "enter the id of tool"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

-- performCommand organizes all the commands the user can enter
performCommand :: String -> IO ()
performCommand command
  | command == "users" = printUsers >> main
  | command == "tools" = printTools >> main
  | command == "adduser" = promptAndAddUser >> main
  | command == "checkout" = promptAndCheckout >> main
  | command == "checkin" = promptAndCheckin >> main
  | command == "in" = printAvailable >> main
  | command == "out" = printCheckedout >> main
  | command == "quit" = print "bye!"
  | otherwise = print "Sorry command not found" >> main

main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command
