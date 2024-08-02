module Game where

import Data.Maybe (Maybe)
import Utils
import Types
import Debug.Trace

import Data.Char (ord)
import Data.List (find)

class Game g where
    newGame :: g
    validateMove :: g -> Move -> Maybe g
    validMoves :: g -> [Move]
    winner :: g -> Maybe Player

initialBoard :: [[Maybe (Player, Piece)]]
initialBoard = let
    whitePawnRow = [Just (White, Pawn) | _ <- [1..8]]
    blackPawnRow = [Just (Black, Pawn) | _ <- [1..8]]
    firstRowGeneric = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    whiteFirstRow = map (\piece -> Just (White, piece)) firstRowGeneric
    blackFirstRow = map (\piece -> Just (Black, piece)) (reverse firstRowGeneric)
    emptyCells = replicate 4 [Nothing | _ <- [1..8]]
    in
    [whiteFirstRow] ++ [whitePawnRow] ++ emptyCells ++ [blackPawnRow] ++ [blackFirstRow]

isCheckmate :: ChessGame -> Player -> Bool
isCheckmate game player =
    isInCheck game player && null (generateAllMoves game player)

isStalemate :: ChessGame -> Bool
isStalemate game =
    not (isInCheck game (playerTurn game)) && null (generateAllMoves game (playerTurn game))

isInCheck :: ChessGame -> Player -> Bool
isInCheck game player =
    let kingSquare = findKing game player
    in any (\opponentMove -> 
        let (Move _ to _) = opponentMove
        in to == kingSquare) (generateAllMoves game (oppositePlayer player))

isQueen :: Maybe (Player, Piece) -> Bool
isQueen Nothing = False
isQueen (Just (_, piece)) = piece == Queen

isGameOver :: ChessGame -> Bool
isGameOver game =
    not (or (map isQueen flattenedBoard))
    where 
        flattenedBoard = flatten (board game) 

instance Game ChessGame where
    newGame = ChessGame {
        board = initialBoard,
        playerTurn = White,
        moveHistory = []
    }

    validateMove game move = 
        if isValidMove game move
        then Just (makeMove game move)
        else Nothing 

    validMoves game = 
        [Move from to promotion | 
         from <- allSquares, 
         to <- allSquares, 
         promotion <- [Nothing, Just Queen, Just Rook, Just Bishop, Just Knight],
         isValidMove game (Move from to promotion)]

    winner game
        | isCheckmate game Black = Just White
        | isCheckmate game White = Just Black
        | otherwise = Nothing

-- Helper functions

oppositePlayer :: Player -> Player
oppositePlayer White = Black
oppositePlayer Black = White

allSquares :: [Square]
allSquares = [Square file rank | file <- ['A'..'H'], rank <- [1..8]]

-- TODO: Add validation that you cannot place a piece where you have a piece already
isValidMove :: ChessGame -> Move -> Bool
isValidMove game (Move from to _) =
    let
        player1 = extractPlayerFromSquare game from
        player2 = extractPlayerFromSquare game to
    in case pieceAt game from of
        Nothing -> False

        Just (player, Knight) ->
            player == playerTurn game &&
            isLegalMove game Knight from to &&
            player1 /= player2

        Just (player, piece) ->
            player == playerTurn game &&
            isLegalMove game piece from to &&
            player1 /= player2 &&
            not (hasBlockers game from to)
            
makeMove :: ChessGame -> Move -> ChessGame
makeMove game (Move from to promotion) =
    ChessGame {
        board = updateBoard (board game) from to promotion,
        playerTurn = oppositePlayer (playerTurn game),
        moveHistory = Move from to promotion : moveHistory game
    }

updateBoard :: [[Maybe (Player, Piece)]] -> Square -> Square -> Maybe Piece -> [[Maybe (Player, Piece)]]
updateBoard board (Square fromFile fromRank) (Square toFile toRank) promotion =
    let fromRow = fromRank - 1
        fromCol = ord fromFile - ord 'A'
        toRow = toRank - 1
        toCol = ord toFile - ord 'A'
        
        piece = board !! fromRow !! fromCol
        
        updatedPiece = case (piece, promotion) of
            (Just (player, Pawn), Just newPiece) | toRank `elem` [1, 8] -> Just (player, newPiece)
            _ -> piece

        updatedBoard = [[ if (r, c) == (toRow, toCol)
                            then updatedPiece
                            else if (r, c) == (fromRow, fromCol)
                            then Nothing
                            else board !! r !! c
                        | c <- [0..7]]
                        | r <- [0..7]]
    in updatedBoard
    
isLegalMove :: ChessGame -> Piece -> Square -> Square -> Bool
isLegalMove game piece (Square fromFile fromRank) (Square toFile toRank) =
    let dx = abs (ord toFile - ord fromFile)
        dy = abs (toRank - fromRank)
    in case piece of
        Pawn -> 
            let direction = if playerTurn game == White then 1 else -1
                startRank = if playerTurn game == White then 2 else 7
            in (dx == 0 && dy == 1 && fromRank + direction == toRank) ||
               (dx == 0 && dy == 2 && fromRank == startRank && fromRank + 2 * direction == toRank) ||
               (dx == 1 && dy == 1 && fromRank + direction == toRank && isOpponentPiece game (Square toFile toRank))
        Knight -> (dx == 1 && dy == 2) || (dx == 2 && dy == 1)
        Bishop -> dx == dy
        Rook -> dx == 0 || dy == 0
        Queen -> dx == dy || dx == 0 || dy == 0
        King -> dx <= 1 && dy <= 1

findKing :: ChessGame -> Player -> Square
findKing game player =
    head [Square (toEnum (ord 'A' + col)) (row + 1) 
          | (row, rank) <- zip [0..] (board game),
            (col, Just (p, King)) <- zip [0..] rank,
            p == player]

generateAllMoves :: ChessGame -> Player -> [Move]
generateAllMoves game player =
    [Move from to promotion |
     from <- allSquares,
     Just (p, piece) <- [pieceAt game from],
     p == player,
     to <- allSquares,
     isValidMove game (Move from to (Just piece)),
     promotion <- if piece == Pawn && getRank to `elem` [1, 8]
                  then [Just Queen, Just Rook, Just Bishop, Just Knight]
                  else [Nothing]]

getRank :: Square -> Int
getRank (Square _ rank) = rank

isOpponentPiece :: ChessGame -> Square -> Bool
isOpponentPiece game square =
    case pieceAt game square of
        Just (player, _) -> player /= playerTurn game
        Nothing -> False

isEmpty :: ChessGame -> Square -> Bool
isEmpty game square = pieceAt game square == Nothing

canMoveTo :: ChessGame -> Square -> Bool
canMoveTo game square =
    isEmpty game square || isOpponentPiece game square

pieceValue :: Piece -> Int
pieceValue Pawn   = 1
pieceValue Knight = 3
pieceValue Bishop = 3
pieceValue Rook   = 5
pieceValue Queen  = 9
pieceValue King   = 9  

evaluatePosition :: Player -> Maybe (Player, Piece) -> Int
evaluatePosition currentPlayer position = 
    case position of 
        Just (positionPlayer, positionPiece) -> if positionPlayer == currentPlayer then pieceValue positionPiece else 0 
        _ -> 0

evaluateBoard :: ChessGame -> Player -> Int
evaluateBoard game current = 
    let
        opponent = switchPlayer current
        currentScore = sum (map (evaluatePosition current) (flatten (board game)))
        opponentScore = sum (map (evaluatePosition opponent) (flatten (board game)))
    in
        currentScore-opponentScore