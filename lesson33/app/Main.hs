module Main where

import           Control.Monad
import           Lib

main :: IO ()
main = someFunc

data Name =
  Name
    { firstName :: String
    , lastName  :: String
    }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel
  = Freshman
  | Sophmore
  | Junior
  | Senior
  deriving (Eq, Ord, Enum, Show)

data Student =
  Student
    { studentId   :: Int
    , gradeLevel  :: GradeLevel
    , studentName :: Name
    }
  deriving (Show)

students :: [Student]
students =
  [ (Student 1 Senior (Name "Audre" "Lorde"))
  , (Student 2 Junior (Name "Leslie" "Silko"))
  , (Student 3 Freshman (Name "Judith" "Butler"))
  , (Student 4 Senior (Name "Guy" "Debord"))
  , (Student 5 Sophmore (Name "Jean" "Baudrillard"))
  , (Student 6 Junior (Name "Julia" "Kristeva"))
  ]

-- Implementing _select
--
-- GHCi> _select (firstName . studentName) students
-- ["Audre","Leslie","Judith","Guy","Jean","Julia"]
--
-- GHCi> :t students
-- students :: [Student]
--
-- GHCi> :t studentName
-- studentName :: Student -> Name
--
-- GHCi> :t firstName
-- firstName :: Name -> String
--
-- GHCi> :t (firstName . studentName)
-- (firstName . studentName) :: Student -> String
--
-- GHCi> _select (\x -> (studentName x, gradeLevel x)) students
-- [(Audre Lorde,Senior),(Leslie Silko,Junior),(Judith Butler,Freshman),(Guy Debord,Senior),(Jean Baudrillard,Sophmore),(Julia Kristeva,Junior)]
--
_select :: (a -> b) -> [a] -> [b]
_select prop vals = do
  val <- vals
  return (prop val)

-- Implementing _where
--
-- GHCi> _where (startsWith 'J' . firstName) (_select studentName students)
-- [Judith Butler,Jean Baudrillard,Julia Kristeva]
--
_where :: (a -> Bool) -> [a] -> [a]
_where test vals = do
  val <- vals
  guard (test val)
  return val

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

-- JOINING COURSE AND TEACHER DATA TYPES
data Teacher =
  Teacher
    { teacherId   :: Int
    , teacherName :: Name
    }
  deriving (Show)

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior"), Teacher 200 (Name "Susan" "Sontag")]

data Course =
  Course
    { courseId    :: Int
    , courseTitle :: String
    , teacher     :: Int
    }
  deriving (Show)

courses :: [Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

--
-- GHCi>  _join teachers courses teacherId teacher
-- [(Teacher {teacherId = 100, teacherName = Simone De Beauvior},Course {courseId = 101, courseTitle = "French", teacher = 100}),(Teacher {teacherId = 200, teacherName = Susan Sontag},Course {courseId = 201, courseTitle = "English", teacher = 200})]
--
_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
  return dpairs

-- BUILDING YOUR HINQ INTERFACE AND EXAMPLE QUERIES
-- Haskellはデフォルト引数をサポートしていない
-- 今のままでは、例えばwhere句が抜けているケースを実現することができない
joinData = _join teachers courses teacherId teacher

whereResult = _where ((== "English") . courseTitle . snd) joinData

selectResult = _select (teacherName . fst) whereResult

_hinq selectQuery joinQuery whereQuery =
  (\joinData -> (\whereResult -> selectQuery whereResult) (whereQuery joinData)) joinQuery

finalResult :: [Name]
finalResult =
  _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName) finalResult (_where (\_ -> True))

-- 33.5. MAKING A HINQ TYPE FOR YOUR QUERIES から


