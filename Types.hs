data Mood = Blah | Woot deriving (Show)

changeMood Woot = Blah
changeMood Blah = Woot

biggerThanFive x =
  if x > 5
    then True
    else False

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhhhh."
  where cool = 
          coolness == "downright frosty yo"

h :: (Num a, Num b) => a -> b -> b
h _a b = b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a _b = a

kessel :: (Ord a, Num b) => a -> b -> a
kessel a _b = a