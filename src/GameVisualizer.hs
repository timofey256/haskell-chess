module GameVisualizer where

import Game
import Types

-- Function to display the game state (board and current player)
displayGame :: ChessGame -> IO ()
displayGame game = do
    putStrLn "Current Board:"
    putStrLn $ show (game :: ChessGame)
    putStrLn $ "Current Player: " ++ show (currentPlayer game)