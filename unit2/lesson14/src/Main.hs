module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data NewEngland = ME | VT | NH | MA | RI | CT

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show)

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- == を定義すれば /= は自明なので不要
instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False

instance Ord SixSidedDie where
  compare S6 S6 = EQ
  compare S6 _ = GT
  compare _ S6 = LT
  compare S5 S5 = EQ
  compare S5 _ = GT
  compare _ S5 = LT

-- instance Enum SixSidedDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "No such value"

--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5


-- fstでソートされてしまう
-- type Name = (String, String)

-- names :: [Name]
-- names = [ ("Emi", "Cioran")
--        , ("Eugene","Thacker")
--        , ("Friedrich","Nietzsche") ]


-- Ordを自分で定義してsndでソートされるようにする
data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [ Name ("Emi", "Cioran")
       , Name ("Eugene","Thacker")
       , Name ("Friedrich","Nietzsche") ]

newtype Name2 = Name2 (String, String) deriving (Show, Eq)

