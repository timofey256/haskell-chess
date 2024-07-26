module Behaviours where

import Types
import Game (makeMove, generateAllMoves, evaluatePosition, isGameOver, isValidMove)
import Data.Char (toUpper)
import Data.List (foldl', maximumBy)
import Data.Ord (comparing)

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
    let depth = 2
    let allMoves = generateAllMoves game (playerTurn game)
    let moves = filter (\x -> isValidMove game x) allMoves 
    putStrLn $ "Number of available moves: " ++ show (length moves)
    case moves of
        [] -> error "No valid moves available. Game might be over."
        _  -> do
            let bestMove = maximumBy (comparing (alphabeta game depth (-1000000) 1000000 True)) moves
            putStrLn $ "Selected move: " ++ show bestMove
            return bestMove
                    
alphabeta :: ChessGame -> Int -> Int -> Int -> Bool -> Move -> Int
alphabeta game depth alpha beta maximizingPlayer move
    | depth == 0 || isGameOver newGame = evaluatePosition newGame
    | maximizingPlayer = maximizer game newGame (depth - 1) alpha beta
    | otherwise = minimizer game newGame (depth - 1) alpha beta
    where newGame = makeMove game move

maxLoop :: ChessGame -> Int -> Int -> Int -> Move -> Int
maxLoop og d beta a m
    | a >= beta = a
    | otherwise =
        let value = alphabeta og d a beta False m
        in max a value

minLoop :: ChessGame -> Int -> Int -> Int -> Move -> Int
minLoop og d alpha b m
    | b <= alpha = b
    | otherwise =
        let value = alphabeta og d alpha b True m
        in min b value

maximizer :: ChessGame -> ChessGame -> Int -> Int -> Int -> Int
maximizer originalGame game depth alpha beta =
    let moves = generateAllMoves game (playerTurn game)
    in if null moves || depth == 0
       then evaluatePosition game
       else foldl' (maxLoop originalGame depth beta) alpha moves

minimizer :: ChessGame -> ChessGame -> Int -> Int -> Int -> Int
minimizer originalGame game depth alpha beta =
    let moves = generateAllMoves game (playerTurn game)
    in if null moves || depth == 0
       then evaluatePosition game
       else foldl' (minLoop originalGame depth alpha) beta moves