module Main where

import Game
import GameVisualizer
import Types
import Behaviours
import System.IO

startGameLoop :: ChessGame -> IO ()
startGameLoop game =
    iterateGameLoop game makeAIMove makeAIMove  -- Assuming both players are manual for now

-- Possible signature would be "ChessGame -> (ChessGame -> Move) -> (ChessGame -> Move) -> IO ()"
-- However, if we get move from IO, we get type mismatch, because decideMove() functions have signature (ChessGame -> IO Move)
iterateGameLoop game whiteDecideMove blackDecideMove = do
    displayGame game
    if isGameOver game
        then putStrLn $ "Game over! Winner: " ++ show (winner game)
        else do
            let currentMakeMoveFunc = if playerTurn game == White then whiteDecideMove else blackDecideMove
            move <- currentMakeMoveFunc game
            case isValidMove game move of
                True -> do
                    let nextStepGame = makeMove game move
                    iterateGameLoop nextStepGame whiteDecideMove blackDecideMove
                False ->  do 
                    putStrLn "\nInvalid move. Try again.\n====================\n"
                    iterateGameLoop game whiteDecideMove blackDecideMove

main :: IO ()
main = do
    putStrLn "Welcome to the Chess Game!"
    startGameLoop newGame