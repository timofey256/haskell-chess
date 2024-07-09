module Main where

import Game
import System.IO

-- Function to display the game state (board and current player)
displayGame :: ChessGame -> IO ()
displayGame game = do
    putStrLn "Current Board:"
    putStrLn $ show (game :: ChessGame)
    putStrLn $ "Current Player: " ++ show (currentPlayer game)

-- Function to get the user's move
getUserMove :: IO Move
getUserMove = do
    putStrLn "Enter your move (e.g., e2 e4):"
    moveInput <- getLine
    let parsedMove = parseMove moveInput
    case parsedMove of
        Just move -> return move
        Nothing   -> do
            putStrLn "Invalid move format. Please try again."
            getUserMove

-- Function to parse a move from user input
parseMove :: String -> Maybe Move
parseMove input =
    case words input of
        [from, to] -> Just $ Move (parseSquare from) (parseSquare to) Nothing
        _ -> Nothing

parseSquare :: String -> Square
parseSquare [file, rank] = Square file (read [rank])
parseSquare _ = error "Invalid square format"

-- Game loop
gameLoop :: ChessGame -> IO ()
gameLoop game = do
    displayGame game
    if isGameOver game
        then putStrLn $ "Game over! Winner: " ++ show (winner game)
        else do
            move <- getUserMove
            case makeMove game move of
                Nothing     -> do
                    putStrLn "Invalid move. Try again."
                    gameLoop game
                Just newGame -> gameLoop newGame

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to the Chess Game!"
    gameLoop newGame
