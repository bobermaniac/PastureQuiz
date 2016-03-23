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
  
class (Food food) => Feeder feeder food where
  feedee :: feeder -> food
  feed :: (Animal animal, PrefferedFood animal ~ food) => feeder -> animal -> String
  feed feeder animal = eat animal (feedee feeder)
  
class (Liquid liquid) => LiquidSource drinker liquid where
  drinkee :: drinker -> liquid
  water :: (Animal animal, PrefferedLiquid animal ~ liquid) => drinker -> animal -> String
  water drinker animal = drink animal (drinkee drinker)

data Pasture = CloverPasture

instance Feeder Pasture Grass where
  feedee CloverPasture = Clover

instance LiquidSource Pasture Water where
  drinkee CloverPasture = StillWater

  