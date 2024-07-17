module Behaviours where

import Types
import Data.Char (toUpper)

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