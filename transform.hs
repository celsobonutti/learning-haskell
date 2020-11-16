module Transform where

  add x = x ++ "!"

  pos x = x !! 4

  remove x = drop 9 x

  thirdLetter x = x !! 2

  rvs = awesome ++ is ++ curry
    where 
      originalSentence = "Curry is awesome"
      awesome = drop 9 originalSentence ++ " "
      curry = take 5 originalSentence
      sentenceBeginning = take 8 originalSentence
      is = drop 6 sentenceBeginning ++ " "