module MyLib (interactiveLines) where

import Data.Char

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

interactiveLines :: Int -> IO ()
interactiveLines counter = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn (show counter ++ "." ++ line)
      interactiveLines (counter + 1)

isEmpty :: String -> Bool
isEmpty str = null str || all (\s -> not $ isPrint s || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty = not . isEmpty

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber lines =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs) =
        let mNumbering = if shouldNumber x then Just counter else Nothing
            newCounter = if shouldIncr x then counter + 1 else counter
         in (mNumbering, x) : go newCounter xs
   in go 1 lines

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty


numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty