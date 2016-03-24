{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Food food where
  foodName :: food -> String

data Grass = Clover String

data Meat = Meat String

instance Food Grass where
  foodName (Clover adj) =  adj ++ " clover"
  
instance Food Meat where
  foodName (Meat adj) = adj ++ " meat"
  
class Liquid liquid where
  liquidName :: liquid -> String
  
data Water = Water String

instance Liquid Water where
  liquidName (Water adj) = adj ++ " water"

class Animal animal where
  type PrefferedFood animal
  type PrefferedLiquid animal
  animalName :: animal -> String
  eat :: Food (PrefferedFood animal) => animal -> PrefferedFood animal -> String
  drink :: Liquid (PrefferedLiquid animal) => animal -> PrefferedLiquid animal -> String
  eat animal food = animalName animal ++ ", eats " ++ foodName food
  drink animal liquid = animalName animal ++ ", drinks " ++ liquidName liquid

data Herbivore = Cow String | Bull String

data Carnivore = Wolf String

instance Animal Herbivore where
  type PrefferedFood Herbivore = Grass
  type PrefferedLiquid Herbivore = Water
  animalName (Cow adj) = adj ++ " cow"
  animalName (Bull adj) = adj ++ " bull"
  
instance Animal Carnivore where
  type PrefferedFood Carnivore = Meat
  type PrefferedLiquid Carnivore = Water
  animalName (Wolf adj) = adj ++ " wolf"
  
class Feeder feeder food where
  supply :: feeder -> food
  on :: feeder -> animal -> (animal -> food -> String) -> String
  on feeder animal consume = consume animal (supply feeder)

data Pasture = CloverPasture

instance Feeder Pasture Grass where
  supply CloverPasture = Clover "fresh"
  
instance Feeder Pasture Water where
  supply CloverPasture = Water "fresh"

data Butchery = Butchery

instance Feeder Butchery Meat where
  supply Butchery = Meat "raw"
