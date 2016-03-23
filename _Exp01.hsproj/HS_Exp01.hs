{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude

class Food food where
  foodName :: food -> String

data Grass = Clover

data Meat = RawMeat

instance Food Grass where
  foodName Clover = "fresh clover"
  
instance Food Meat where
  foodName RawMeat = "some raw meat"
  
class Liquid liquid where
  liquidName :: liquid -> String
  
data Water = StillWater

instance Liquid Water where
  liquidName StillWater = "still water"

class Animal animal where
  type PrefferedFood animal
  type PrefferedLiquid animal
  animalName :: animal -> String
  eat :: Food (PrefferedFood animal) => animal -> PrefferedFood animal -> String
  drink :: Liquid (PrefferedLiquid animal) => animal -> PrefferedLiquid animal -> String
  eat animal food = animalName animal ++ ", eats " ++ foodName food
  drink animal liquid = animalName animal ++ ", drinks " ++ liquidName liquid

data Herbivore = Cow | Bull

data Carnivore = Wolf

instance Animal Herbivore where
  type PrefferedFood Herbivore = Grass
  type PrefferedLiquid Herbivore = Water
  animalName Cow = "cow"
  animalName Bull = "bull"
  
instance Animal Carnivore where
  type PrefferedFood Carnivore = Meat
  type PrefferedLiquid Carnivore = Water
  animalName Wolf = "wolf"
  
class Feeder feeder food where
  supply :: feeder -> food
  feed :: feeder -> animal -> (animal -> food -> String) -> String
  feed feeder animal consume = consume animal (supply feeder)

data Pasture = CloverPasture

instance Feeder Pasture Grass where
  supply CloverPasture = Clover
  
instance Feeder Pasture Water where
  supply CloverPasture = StillWater

data Butchery = Butchery

instance Feeder Butchery Meat where
  supply Butchery = RawMeat

  