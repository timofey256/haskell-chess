module Behaviours where

import Types
import Game (makeMove, generateAllMoves, evaluateBoard, isGameOver, isValidMove)
import Data.Char (toUpper)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Utils
import Debug.Trace (trace)

parseMove :: String -> Maybe Move
parseMove input =
    case words input of
        [from, to] -> Just $ Move (parseSquare from) (parseSquare to) Nothing
        _ -> Nothing

parseSquare :: String -> Square
parseSquare [file, rank] = Square (toUpper file) (read [rank])
parseSquare _ = error "Invalid square format"

makeManualMove :: ChessGame -> IO Move
makeManualMove game = do
    putStrLn "Enter your move (e.g., e2 e4):"
    moveInput <- getLine
    let parsedMove = parseMove moveInput
    case parsedMove of
        Just move -> return move
        Nothing   -> do
            putStrLn "Invalid move format. Please try again."
            makeManualMove game

makeAIMove :: ChessGame -> IO Move
makeAIMove game = do
    putStrLn $ "Current player: " ++ show (playerTurn game)
    let depth = 4 -- should be even number!
    let allMoves = generateAllMoves game (playerTurn game)
    let moves = filter (\x -> isValidMove game x) allMoves 
    putStrLn $ "Number of available moves: " ++ show (length moves)
    case moves of
        [] -> error "No valid moves available. Game might be over."
        _  -> do
            let bestMove = minimumBy (comparing (alphabeta game (playerTurn game) depth (-1000000) 1000000 True)) moves
            putStrLn $ "Selected move: " ++ show bestMove
            return bestMove
                    
alphabeta :: ChessGame -> Player -> Int -> Int -> Int -> Bool -> Move -> Int
alphabeta game currentPlayer depth alpha beta maximizingPlayer move
    | depth == 0 || isGameOver potentialGame = evaluateBoard potentialGame currentPlayer
    | maximizingPlayer = maximizer game potentialGame currentPlayer (depth - 1) alpha beta 
    | otherwise = minimizer game potentialGame currentPlayer (depth - 1) alpha beta
    where potentialGame = makeMove game move

maxLoop :: ChessGame -> Player -> Int -> Int -> Int -> Move -> Int
maxLoop og currentPlayer d beta a m
    | a >= beta = a
    | otherwise =
        let value = alphabeta og (switchPlayer currentPlayer) d a beta False m
        in max a value

minLoop :: ChessGame -> Player -> Int -> Int -> Int -> Move -> Int
minLoop og currentPlayer d alpha b m
    | b <= alpha = b
    | otherwise =
        let value = alphabeta og (switchPlayer currentPlayer) d alpha b True m
        in min b value

maximizer :: ChessGame -> ChessGame -> Player -> Int -> Int -> Int -> Int
maximizer originalGame game currentPlayer depth alpha beta =
    let moves = generateAllMoves game (playerTurn game)
    in if null moves || depth == 0
       then evaluateBoard game currentPlayer
       else foldl' (maxLoop originalGame currentPlayer depth beta) alpha moves

minimizer :: ChessGame -> ChessGame -> Player -> Int -> Int -> Int -> Int
minimizer originalGame game currentPlayer depth alpha beta =
    let moves = generateAllMoves game (playerTurn game)
    in if null moves || depth == 0
       then evaluateBoard game currentPlayer
       else foldl' (minLoop originalGame currentPlayer depth alpha) beta moves