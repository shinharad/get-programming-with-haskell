module Main where

import           Control.Applicative
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
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
  val <- vals
  return (prop val)

-- Implementing _where
--
-- GHCi> _where (startsWith 'J' . firstName) (_select studentName students)
-- [Judith Butler,Jean Baudrillard,Julia Kristeva]
--
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
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
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
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

-- MAKING A HINQ TYPE FOR YOUR QUERIES
data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

--
-- GHCi> runHINQ query1
-- [Susan Sontag]
--
query1 :: HINQ [] (Teacher, Course) Name
query1 =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

--
-- GHCi> runHINQ query2
-- [Simone De Beauvior,Susan Sontag]
--
query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

-- Using HINQ with Maybe types
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

--
-- GHCi> runHINQ maybeQuery1
-- Just Simone De Beauvior
--
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher possibleCourse teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

--
-- GHCi> runHINQ maybeQuery2
-- Nothing
--
maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher missingCourse teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

-- Joining multiple lists to get all enrollments
data Enrollment =
  Enrollment
    { student :: Int
    , course  :: Int
    }
  deriving (Show)

enrollments :: [Enrollment]
enrollments =
  [ (Enrollment 1 101)
  , (Enrollment 2 101)
  , (Enrollment 2 201)
  , (Enrollment 3 101)
  , (Enrollment 4 201)
  , (Enrollment 4 101)
  , (Enrollment 5 101)
  , (Enrollment 6 201)
  ]

studentEnrollmentsQ =
  HINQ_ (_select (\(st, en) -> (studentName st, course en))) (_join students enrollments studentId student)

--
-- GHCi> studentEnrollments
-- [(Audre Lorde,101),(Leslie Silko,101),(Leslie Silko,201),(Judith Butler,101),(Guy Debord,201),(Guy Debord,101),(Jean Baudrillard,101),(Julia Kristeva,201)]
--
studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ =
  HINQ
    (_select (fst . fst))
    (_join studentEnrollments courses snd courseId)
    (_where ((== "English") . courseTitle . snd))

--
-- GHCi> englishStudents
-- [Leslie Silko,Guy Debord,Julia Kristeva]
--
englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

--
-- GHCi> getEnrollments "English"
-- [Leslie Silko,Guy Debord,Julia Kristeva]
--
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery =
      HINQ
        (_select (fst . fst))
        (_join studentEnrollments courses snd courseId)
        (_where ((== courseName) . courseTitle . snd))
