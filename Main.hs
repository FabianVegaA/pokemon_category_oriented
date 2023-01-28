{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

main :: IO ()
main = do
  print s1
  print s2

data Pokemon = Pokemon
  { namePokemon :: String,
    typePokemon :: Type,
    healthPoints :: Int,
    attack :: Int,
    defense :: Int,
    speed :: Int,
    movements :: [Movement]
  }
  deriving (Show)

data Type
  = Fire
  | Water
  | Grass
  | Electric
  | Normal
  deriving (Show, Eq)

data Movement = Movement
  { nameMovement :: String,
    typeMovement :: Type,
    power :: Int,
    accuracy :: Int,
    movement :: Pokemon -> Pokemon
  }

instance Show Movement where
  show m = nameMovement m

data Trainer = Trainer
  { nameTrainer :: String,
    pokemons :: [Pokemon]
  }
  deriving (Show)

data Attack = Attack
  { attacker :: Pokemon,
    defender :: Pokemon,
    mov :: Movement
  }

data CombatState a b = CombatState
  { fighter1 :: a,
    fighter2 :: b
  }
  deriving (Show)

instance Functor (CombatState a) where

  fmap m (CombatState f1 f2) = CombatState f1 (m f2)

charizar :: Pokemon
charizar =
  Pokemon
    { namePokemon = "Charizar",
      typePokemon = Fire,
      healthPoints = 78,
      attack = 84,
      defense = 78,
      speed = 100,
      movements =
        [ Movement
            { nameMovement = "Scratch",
              typeMovement = Normal,
              power = 40,
              accuracy = 100,
              movement = \p -> p {healthPoints = healthPoints p - 40}
            }
        ]
    }

bulbasaur :: Pokemon
bulbasaur =
  Pokemon
    { namePokemon = "Bulbasaur",
      typePokemon = Grass,
      healthPoints = 45,
      attack = 49,
      defense = 49,
      speed = 45,
      movements =
        [ Movement
            { nameMovement = "Tackle",
              typeMovement = Normal,
              power = 40,
              accuracy = 100,
              movement = \p -> p {healthPoints = healthPoints p - 41}
            }
        ]
    }

s1 = CombatState charizar bulbasaur

s2 =
  let attackCharizar = head . movements $ charizar
   in fmap (movement attackCharizar) s1
