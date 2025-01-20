import Data.List
import Data.Ord
import Data.Maybe (fromMaybe)
import System.IO (readFile, hFlush, stdout)
import Control.Concurrent (threadDelay, forkIO)

-- to check if the heart is still beating
heartbeat :: IO ()
heartbeat = do
    putStrLn "Program is still running..."
    hFlush stdout  -- Force flushing the output buffer
    threadDelay (60 * 1000000)  -- Wait for 60 seconds
    heartbeat


main :: IO ()
main = do
    _ <- forkIO heartbeat  -- Run heartbeat in a separate thread
    -- The heart!
    heart



-- Function to read a file and split it into emails using '~~~' as delimiter
readCorpusFromFile :: FilePath -> IO [String]
readCorpusFromFile path = do
    content <- readFile path
    -- Split by '~~~' and trim leading/trailing whitespace from each email
    return (map (unwords . words . trim) (splitOn "~~~" content))

-- Helper function to trim leading and trailing whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Helper function to split a string on a delimiter
splitOn :: String -> String -> [String]
splitOn _ "" = []
splitOn delimiter str = let (before, remainder) = breakOn delimiter str
                        in before : case remainder of
                                      "" -> []
                                      _  -> splitOn delimiter (drop (length delimiter) remainder)

-- Function to break a string on the first occurrence of a delimiter
breakOn :: String -> String -> (String, String)
breakOn _ [] = ([], [])
breakOn delimiter str@(x:xs)
    | take (length delimiter) str == delimiter = ([], str)
    | otherwise = let (before, after) = breakOn delimiter xs
                  in (x:before, after)

-- Main function to load corpora and compute probabilities
heart :: IO ()
heart = do
    -- Read the spam and non-spam corpora from text files
    spamCorpus <- readCorpusFromFile "/home/students/arjuna.ug2024/Desktop/spam/spam.txt"
    unSpamCorpus <- readCorpusFromFile "/home/students/arjuna.ug2024/Desktop/spam/notSpam.txt"

    let spamMailCount = length spamCorpus
        unSpamMailCount = length unSpamCorpus

        globalProbSpam = fromIntegral spamMailCount / fromIntegral (spamMailCount + unSpamMailCount)

        -- Count occurrences of words in a list
        countInList [] = []
        countInList (x:xs) = go (x:xs) []
          where
            go [] counts = counts
            go (y:ys) counts
              | y `elem` map fst counts = go ys (map (\(w, c) -> if w == y then (w, c + 1) else (w, c)) counts)
              | otherwise = go ys ((y, 1) : counts)

        -- Create a list of word counts from the spam and unspam corpora
        spamList = countInList $ concatMap words spamCorpus
        unSpamList = countInList $ concatMap words unSpamCorpus

        spamWordCount = sum (map snd spamList)
        unSpamWordCount = sum (map snd unSpamList)

        -- Calculate probability for each word being in spam based on counts in spam and unspam corpora
        probWordSpam [] _ = []
        probWordSpam ((word, countInSpam):xs) unSpamWordsCounts =
            let countInUnSpam = fromMaybe 0 (lookup word unSpamWordsCounts)
                probWord = if countInSpam + countInUnSpam > 0
                           then (fromIntegral countInSpam / fromIntegral spamWordCount) *
                                (fromIntegral spamMailCount / fromIntegral (spamMailCount + unSpamMailCount)) /
                                (fromIntegral countInSpam + fromIntegral countInUnSpam)
                           else 0
            in (word, probWord) : probWordSpam xs unSpamWordsCounts

        -- Apply probability calculation for words found in spam
        wordProbabilities = probWordSpam spamList unSpamList

    -- Print the word probabilities
    print $ sortBy (flip (comparing snd)) $ wordProbabilities