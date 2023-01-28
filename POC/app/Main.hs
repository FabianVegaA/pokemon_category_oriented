module Main where

import Data.Map (elemAt, (!))
import Lib
  ( CombatState (..),
    Movement (..),
    Pokemon (..),
    applyDamage,
    isOver,
    pokemonList,
    swapCombatState,
  )
import System.Random (randomRIO)

listMovements :: [Movement] -> String
listMovements ms = unlines . zipWith (\i m -> show i ++ ". " ++ show m) [0 ..] $ ms

main :: IO ()
main = do
  let charizar = pokemonList ! "Charizar"

  i <- randomRIO (0, length pokemonList - 1)
  let opponent = snd $ elemAt i pokemonList

  putStrLn $ namePokemon charizar ++ " vs " ++ namePokemon opponent
  putStrLn "-------------------------------------"
  let combat = CombatState charizar opponent
  fight combat True

fight :: CombatState Pokemon Pokemon -> Bool -> IO ()
fight combat@(CombatState f1 f2) turn1
  | isOver f1 = putStrLn $ "You lose! The winner is " ++ namePokemon f2
  | isOver f2 = putStrLn $ "You win! The winner is " ++ namePokemon f1
  | otherwise = do
      let player = if turn1 then f1 else f2
      putStrLn $ "Stats: " ++ show player ++ " | " ++ show (if turn1 then f2 else f1)
      nextCombat <-
        if turn1
          then play combat
          else playAutoOpponent combat
      fight nextCombat (not turn1)

play :: CombatState Pokemon Pokemon -> IO (CombatState Pokemon Pokemon)
play (CombatState a d) = do
  putStrLn $ "Movements: \n" ++ listMovements (movements a)
  putStrLn "Choose a movement: "
  i <- read <$> getLine
  let mov = (!! i) . movements $ a
  return $ applyDamage mov (CombatState a d)

playAutoOpponent :: CombatState Pokemon Pokemon -> IO (CombatState Pokemon Pokemon)
playAutoOpponent (CombatState d a) = do
  i <- randomRIO (0, length (movements a) - 1)
  let mov = movements a !! i
  putStrLn $ "The opponent choose " ++ show mov
  return . swapCombatState $ applyDamage mov (CombatState a d)
