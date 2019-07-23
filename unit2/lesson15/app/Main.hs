module Main where

main :: IO ()
main = return ()

data FourLetterAlphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

-- A generic rotN function to work on any alphabet
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

-- GHCi> rotN 4 L1
-- L3
-- GHCi> rotN 4 L2
-- L4
-- GHCi> rotN 4 L3
-- L1
-- GHCi> rotN 4 L4
-- L2
-- Getting the number representing the largest Char
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

-- Rotating a single Char
rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where
    sizeOfAlphabet = 1 + largestCharNumber
  -- where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- A message in your four-letter alphabet
message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

-- Defining a fourLetterEncoder with map
fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot41 vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot41 = rotN alphaSize -- partial application

-- A three-letter alphabet, message, and encoder
data ThreeLetterAlphabet
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot31 vals
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot31 = rotN alphaSize

-- A rotNdecoder that works with odd-numbered alphabets
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else 1 + fromEnum c + halfN
    rotation = offset `mod` n

-- A working decoder for ThreeLetterAlphabet
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot31decoder vals
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot31decoder = rotNdecoder alphaSize
