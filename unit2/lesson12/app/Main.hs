module Main where

main :: IO ()
main = return ()

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

main1 = do
  print $ patientInfo "John" "Doe" 43 74
    -- "Doe, John (43yrs. 74in.)"
  print $ patientInfo "Jane" "Smith" 25 62
    -- "Smith, Jane (25yrs. 62in.)"

-- ----------------------------------------------
type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

patientInfo2 :: FirstName -> LastName -> Age -> Height -> String
patientInfo2 fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo3 :: PatientName -> Age -> Height -> String
patientInfo3 (fname, lname) age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- ----------------------------------------------
data Sex
  = Male
  | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

-- ----------------------------------------------
data RhType
  = Pos
  | Neg

data ABOType
  = A
  | B
  | AB
  | O

data BloodType =
  BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True
canDonateTo _ (BloodType AB _)              = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False

main2 = do
  print $ canDonateTo patient1BT patient2BT -- False
  print $ canDonateTo patient2BT patient1BT -- True
  print $ canDonateTo patient2BT patient3BT -- True
  print $ canDonateTo patient1BT patient3BT -- True
  print $ canDonateTo patient3BT patient1BT -- False

-- ----------------------------------------------
type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient =
  Patient
    { name      :: Name
    , sex       :: Sex
    , age       :: Int
    , height    :: Int
    , weight    :: Int
    , bloodType :: BloodType
    }

jackieSmith :: Patient
jackieSmith =
  Patient {name = Name "Jackie" "Smith", age = 43, sex = Female, height = 62, weight = 115, bloodType = BloodType O Neg}

main3 = do
  print $ height jackieSmith -- 62
  print $ showBloodType (bloodType jackieSmith) -- "O-"
