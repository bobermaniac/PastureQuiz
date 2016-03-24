{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

import Prelude
import GHC.Prim

-- Consumable classes

class Consumable c' where
  consumableName :: c' -> String
  consumableName _ = "unknown consumable"
  
class (Consumable c') => Plant c'

class (Consumable c') => Flesh c'

-- Consumable instances

data Grass = Clover | MeadowGrass

instance Consumable Grass where
  consumableName Clover = "clover"
  consumableName MeadowGrass = "meadow grass"
  
instance Plant Grass

data Meat = Meat

instance Consumable Meat where
  consumableName Meat = "some meat"
  
instance Flesh Meat

-- Liquid classes

class Liquid l' where
  liquidName :: l' -> String
  liquidName _ = "unknown liquid"

-- Liquid instances

data Water = Water

instance Liquid Water where
  liquidName Water = "water"

-- Animal class

class Animal a' where
  animalName :: a' -> String
  type Food a' c' :: Constraint
  type Drink a' l' :: Constraint
  accept :: a' -> supply' -> (supply' -> String) -> String -> String
  eat :: (Consumable f', Food a' f') => a' -> f' -> String
  drink :: (Liquid l', Drink a' l') => a' -> l' -> String
  type Food a' c' = Consumable c'
  type Drink a' l' = Liquid l'
  accept animal supply transform joint = animalName animal ++ ", " ++ joint ++ " " ++ transform supply
  eat animal food = accept animal food consumableName "eat"
  drink animal liquid = accept animal liquid liquidName "drink"
  
-- Animal instances

data Herbivore = Cow

instance Animal Herbivore where
  animalName Cow = "cow"
  type Food Herbivore c' = Plant c'
  
instance Consumable Herbivore where
  consumableName Cow = "beef"
  
instance Flesh Herbivore

data Carnivore = Wolf

instance Animal Carnivore where
  animalName Wolf = "wolf"
  type Food Carnivore c' = Flesh c'
  
instance Consumable Carnivore where
  consumableName Wolf = "wolf's meat"
  
instance Flesh Carnivore 