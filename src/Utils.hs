module Utils where

import Types
import Data.Char

pieceAt :: ChessGame -> Square -> Maybe (Player, Piece)
pieceAt game (Square file rank) = 
    board game !! (rank - 1) !! (ord file - ord 'A')

getElement :: (Int, Int) -> [[a]] -> a
getElement (i, j) list = (list !! j) !! i

fromSquareToPositionTuple :: Square -> (Int, Int)
fromSquareToPositionTuple (Square file rank) = 
    (ord file - ord 'A', rank-1)

extractPlayer :: Maybe (Player, Piece) -> Maybe Player
extractPlayer (Just (player, _)) = Just player
extractPlayer Nothing = Nothing

extractPlayerFromSquare :: ChessGame -> Square -> Maybe Player
extractPlayerFromSquare game square =
    let
        positionIndices = fromSquareToPositionTuple square
        position = getElement positionIndices (board game)
    in
        extractPlayer position

hasBlockers :: ChessGame -> Square -> Square -> Bool
hasBlockers game (Square c1 r1) (Square c2 r2) =
    let path = pathBetween (Square c1 r1) (Square c2 r2)
    in any (isBlocked game) path

pathBetween :: Square -> Square -> [Square]
pathBetween (Square c1 r1) (Square c2 r2)
    | c1 == c2  = [Square c1 r | r <- range r1 r2]  -- vertical move
    | r1 == r2  = [Square c r1 | c <- range c1 c2]  -- horizontal move
    | abs (ord c1 - ord c2) == abs (r1 - r2) = [Square c r | (c, r) <- zip (range c1 c2) (range r1 r2)]  -- diagonal move
    | otherwise = []

-- Helper function to create a range between two characters or integers
range :: (Enum a, Ord a) => a -> a -> [a]
range x y
    | x < y = [(succ x)..(pred y)]
    | otherwise = [(succ y)..(pred x)]

isBlocked :: ChessGame -> Square -> Bool
isBlocked game (Square c r) = 
    case board game !! (r - 1) !! (ord c - ord 'A') of
        Just _  -> True
        Nothing -> False