module Types where

data Player = White | Black deriving (Show, Eq)
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Square = Square Char Int deriving (Show, Eq)
data Move = Move Square Square (Maybe Piece) deriving (Show, Eq)

data ChessGame = ChessGame {
    board :: [[Maybe (Player, Piece)]],
    playerTurn :: Player,
    moveHistory :: [Move]
} deriving (Show)