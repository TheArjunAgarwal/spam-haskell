import Data.List
import Data.Maybe (fromMaybe)

spamCorpus :: [String]
spamCorpus = ["give me money"]

spamMailCount :: Int
spamMailCount = length spamCorpus

unSpamCorpus :: [String]
unSpamCorpus = ["Hello bro!"]

unSpamMailCount :: Int
unSpamMailCount = length unSpamCorpus

globalProbSpam :: Float
globalProbSpam = fromIntegral spamMailCount / fromIntegral (spamMailCount + unSpamMailCount)

-- Count occurrences of words in a list
countInList :: Eq a => [a] -> [(a, Int)]
countInList [] = []
countInList (x:xs) = go (x:xs) []
  where
    go [] counts = counts
    go (y:ys) counts
      | y `elem` map fst counts = go ys (map (\(w, c) -> if w == y then (w, c + 1) else (w, c)) counts)
      | otherwise = go ys ((y, 1) : counts)

-- Create a list of word counts from the spam and unspam corpora
spamList :: [(String, Int)]
spamList = countInList $ concatMap words spamCorpus

unSpamList :: [(String, Int)]
unSpamList = countInList $ concatMap words unSpamCorpus

spamWordCount :: Int
spamWordCount = sum (map snd spamList)

unSpamWordCount :: Int
unSpamWordCount = sum (map snd unSpamList)

-- Calculate probability for each word being in spam based on counts in spam and unspam corpora
probWordSpam :: [(String, Int)] -> [(String, Int)] -> [(String, Float)]
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
wordProbabilities :: [(String, Float)]
wordProbabilities = probWordSpam spamList unSpamList

main = print $ wordProbabilities