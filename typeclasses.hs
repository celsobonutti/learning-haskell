data Trivial
  = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun

data Date
  = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Show DayOfWeek where
  show Mon = "Monday"
  show Tue = "Tuesday"
  show Weds = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"
  show Sat = "Saturday"
  show Sun = "Sunday"

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') =
      weekday == weekday'
        && dayOfMonth == dayOfMonth'

instance Show Date where
  show (Date weedkay dayOfMonth) =
    show weedkay ++ ", " ++ show dayOfMonth

data Identity a
  = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger
  = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn int) (TisAn int') = int == int'

data TwoIntegers
  = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = (a == a') && (b == b')

data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAnInt int') = int == int'
  (==) (TisAString str) (TisAString str') = str == str'
  (==) _ _ = False

data Pair a
  = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = (a == a') && (b == b')

data Tuple a b
  = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) a b = case (a, b) of
    (Hello x, Hello x') -> x == x'
    (Goodbye x, Goodbye x') -> x == x'
    _ -> False