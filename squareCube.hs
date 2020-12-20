module SquareCube where

mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]

squareCube = [(x, y) | x <- mySqr, x < 50 ,y <- myCube, y < 50]

countTuples = length squareCube
