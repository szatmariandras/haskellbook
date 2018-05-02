module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

main :: IO ()
main = do
  putStrLn "hello world"

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in     l >= minWordLength
                && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
    Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
         ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar Just a  = a

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (take (length word) $ repeat Nothing) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word discovered guessed) c
    | elem c guessed    = p
    | not (elem c word) = Puzzle word discovered $ guessed ++ [c]
    | otherwise         =
        let
            newDiscovered =
                map (\(a,b) -> if a == c then Just a else b) $ zip word discovered
        in Puzzle word newDiscovered $ guessed ++ [c]

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that\
                    \ character, pick \
                    \ something else!"
            return puzzle
        (True, _) ->
            putStrLn "This character was in the\
                    \ word, filling the work\
                    \ accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in\
                    \ the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 7 then
        do
            putStrLn "You lose!"
            putStrLn $
                "The word was: " ++ wordToGuess
            exitSuccess
    else return ()
