module GameVisualizer where

import Game
import Types

-- Function to display the game state (board and current player)
-- Convert a piece to a string
pieceToString :: Maybe (Player, Piece) -> String
pieceToString Nothing = "."
pieceToString (Just (player, piece)) = show player ++ " " ++ show piece

-- Convert a row to a string
rowToString :: [Maybe (Player, Piece)] -> String
rowToString row = unwords $ map pieceToString row

-- Print the game
displayGame :: ChessGame -> IO ()
displayGame game = do
    putStrLn "Current Board:"
    mapM_ (putStrLn . rowToString) (board game)
    putStrLn $ "Current Player: " ++ show (playerTurn game)