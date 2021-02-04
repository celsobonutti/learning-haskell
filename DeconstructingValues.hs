{-# LANGUAGE NamedFieldPuns #-}

module DeconstructingValues where

newtype Name = Name String deriving (Show)

newtype Acres = Acres Int deriving (Show)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer
  = Farmer Name Acres FarmerType
  deriving (Show)

data FarmerRec = FarmerRec
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  }
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec FarmerRec{farmerType} =
  case farmerType of
    DairyFarmer -> True
    _ -> False
