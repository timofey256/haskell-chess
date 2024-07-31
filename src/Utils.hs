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

isBlocked :: ChessGame -> Square -> Bool
isBlocked game square = 
    piece /= Nothing
    where 
        piece = pieceAt game square

noBlockers' :: ChessGame -> Square -> Square -> Bool
noBlockers' game (Square c1 r1) (Square c2 r2) =
    if c1 == c2 then
        [Square c1 r | r <- range r1 r2]  -- Vertical move
    else if r1 == r2 then
        [Square c r1 | c <- range (ord c1) (ord c2)]  -- Horizontal move
    else if abs (ord c1 - ord c2) == abs (r1 - r2) then
        [Square (chr c) r | (c, r) <- zip (range (ord c1) (ord c2)) (range r1 r2)]  -- Diagonal move
    else
        []
    where
        range x y
            | x < y     = [x+1..y-1]  -- Exclude the 'from' and 'to' squares
            | otherwise = reverse [y+1..x-1]  -- Handle reverse range

-- Check if there are no blockers from one square to another
noBlockers :: Square -> Square -> Bool
noBlockers from to = all (not . isBlocked) (noBlockers' from to)