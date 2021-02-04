module ConstructingValues where

data GuessWhat
  = ChickenButt
  deriving (Eq, Show)

data Id a
  = MkId a
  deriving (Eq, Show)

data Product a b
  = Product a b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfrist  :: a
  , psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow
  = NumCow Int
  deriving (Eq, Show)

newtype NumPig
  = NumPig Int
  deriving (Eq, Show)

data Farmhouse
  = Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep
  = NumSheep Int
  deriving (Eq, Show)

data BigFarmHouse
  = BigFarmHouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmHouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo
  = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo
  = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo
  = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os   :: OperatingSystem
  , lang :: ProgLang
  }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = os, lang = lang} | os <- allOperatingSystems, lang <- allLanguages]
