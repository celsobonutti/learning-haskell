module Main where

import Validation ( Validation(..) )

main = do
  let failure ::
        String ->
        Validation String Int
      failure = Failure
      success ::
        Int ->
        Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2