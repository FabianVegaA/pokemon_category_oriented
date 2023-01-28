module Lib
  ( CombatState (..),
    Pokemon (..),
    Type (..),
    Movement (..),
    swapCombatState,
    applyDamage,
    isOver,
    pokemonList,
  )
where

import Data.Map (fromList, (!))

data Pokemon = Pokemon
  { namePokemon :: String,
    typePokemon :: Type,
    healthPoints :: Int,
    attack :: Int,
    defense :: Int,
    speed :: Int,
    movements :: [Movement]
  }

instance Show Pokemon where
  show p = "Pokemon: " ++ namePokemon p ++ " HP: " ++ show (healthPoints p) ++ " Type: " ++ show (typePokemon p)

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
    effect :: Movement -> Pokemon -> Pokemon -> Pokemon
  }

instance Show Movement where
  show m = nameMovement m

data CombatState attacker defender = CombatState attacker defender

instance (Show a, Show b) => Show (CombatState a b) where
  show (CombatState f1 f2) = "CombatState: " ++ show f1 ++ " vs " ++ show f2

instance Functor (CombatState a) where
  fmap m (CombatState f1 f2) = CombatState f1 (m f2)

swapCombatState :: CombatState Pokemon Pokemon -> CombatState Pokemon Pokemon
swapCombatState (CombatState f1 f2) = CombatState f2 f1

isOver :: Pokemon -> Bool
isOver p = healthPoints p <= 0

setMovements :: [String] -> [Movement]
setMovements = map (movementList !)

effectiveness :: Type -> Type -> Float
effectiveness Fire Water = 0.5
effectiveness Fire Grass = 2
effectiveness Water Fire = 2
effectiveness Water Grass = 0.5
effectiveness Grass Fire = 0.5
effectiveness Grass Water = 2
effectiveness _ _ = 1

calculateDamage :: Movement -> Pokemon -> Pokemon -> Pokemon
calculateDamage m p1 p2 =
  let atk = fromIntegral $ attack p1
      def = fromIntegral $ defense p2
      pow = fromIntegral $ power m
      eff = effectiveness (typeMovement m) (typePokemon p2)
      damage = round $ (0.5 * atk * pow / def + 2) * eff
   in p2 {healthPoints = max 0 (healthPoints p2 - damage)}

applyDamage :: Movement -> CombatState Pokemon Pokemon -> CombatState Pokemon Pokemon
applyDamage m c@(CombatState p1 _) = fmap ((effect m) m p1) c

pokemonList =
  fromList $
    map
      (\p -> (namePokemon p, p))
      [ ( Pokemon
            { namePokemon = "Charizar",
              typePokemon = Fire,
              healthPoints = 78,
              attack = 84,
              defense = 78,
              speed = 100,
              movements = setMovements ["Scratch", "Ember", "Fire Spin"]
            }
        ),
        ( Pokemon
            { namePokemon = "Bulbasaur",
              typePokemon = Grass,
              healthPoints = 45,
              attack = 49,
              defense = 49,
              speed = 45,
              movements = setMovements ["Tackle", "Vine Whip"]
            }
        ),
        ( Pokemon
            { namePokemon = "Squirtle",
              typePokemon = Water,
              healthPoints = 44,
              attack = 48,
              defense = 65,
              speed = 43,
              movements = setMovements ["Tackle", "Water Gun"]
            }
        ),
        ( Pokemon
            { namePokemon = "Pikachu",
              typePokemon = Electric,
              healthPoints = 35,
              attack = 55,
              defense = 40,
              speed = 90,
              movements = setMovements ["Tackle", "Thunder Shock"]
            }
        )
      ]

movementList =
  fromList $
    map
      (\p -> (nameMovement p, p))
      [ ( Movement
            { nameMovement = "Scratch",
              typeMovement = Normal,
              power = 40,
              accuracy = 100,
              effect = calculateDamage
            }
        ),
        ( Movement
            { nameMovement = "Tackle",
              typeMovement = Normal,
              power = 40,
              accuracy = 100,
              effect = calculateDamage
            }
        ),
        ( Movement
            { nameMovement = "Ember",
              typeMovement = Fire,
              power = 40,
              accuracy = 100,
              effect = calculateDamage
            }
        ),
        ( Movement
            { nameMovement = "Vine Whip",
              typeMovement = Grass,
              power = 45,
              accuracy = 100,
              effect = calculateDamage
            }
        ),
        ( Movement
            { nameMovement = "Fire Spin",
              typeMovement = Fire,
              power = 100,
              accuracy = 85,
              effect = \mov p1 p2 -> p2 {healthPoints = healthPoints (calculateDamage mov p1 p2), defense = defense p2 - 10}
            }
        ),
        ( Movement
            { nameMovement = "Water Gun",
              typeMovement = Water,
              power = 40,
              accuracy = 100,
              effect = calculateDamage
            }
        ),
        ( Movement
            { nameMovement = "Thunder Shock",
              typeMovement = Electric,
              power = 40,
              accuracy = 100,
              effect = calculateDamage
            }
        )
      ]
