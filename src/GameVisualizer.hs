module GameVisualizer where

import Game
import Types
import Data.Maybe (fromMaybe)
import Data.Char

-- Convert a piece to a string representation
pieceToChar :: (Player, Piece) -> Char
pieceToChar (player, piece) = 
    let pieceChar = case piece of
            Pawn -> 'P'
            Knight -> 'N'
            Bishop -> 'B'
            Rook -> 'R'
            Queen -> 'Q'
            King -> 'K'
    in if player == White then pieceChar else toLower pieceChar

-- Convert a square to a string
squareToString :: Maybe (Player, Piece) -> String
squareToString = maybe "." (return . pieceToChar)

-- Convert a row to a string
rowToString :: [Maybe (Player, Piece)] -> String
rowToString row = unwords (map squareToString row) ++ "\n"

-- Convert the entire board to a string
boardToString :: [[Maybe (Player, Piece)]] -> String
boardToString board = 
    let 
        rows = map rowToString (reverse board)
        rankLabels = map show (reverse [1..8])
        labeledRows = zipWith (\label row -> label ++ " " ++ row) rankLabels rows
        fileLabels = "  a b c d e f g h\n"
    in unlines labeledRows ++ fileLabels

-- Pretty print the entire chess game
prettyPrintChessGame :: ChessGame -> String
prettyPrintChessGame game =
    let 
        boardStr = boardToString (board game)
        turnStr = "Current turn: " ++ show (playerTurn game) ++ "\n"
        movesStr = "Number of moves: " ++ show (length (moveHistory game)) ++ "\n"
    in boardStr ++ turnStr ++ movesStr

-- Function to display the game
displayGame :: ChessGame -> IO ()
displayGame = putStr . prettyPrintChessGame