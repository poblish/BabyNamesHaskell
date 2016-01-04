import Data.List

foreNames = sort ["Rohan","Nathaniel","Anthony","Chris","Jonathan","Lemur","Harry","Percy","Peregrine","James","Jamie","Sidney","Gabriel","Leyton","Curtley","Jarvis"]
middleNames = sort ["Rohan","Nathaniel","Tony","Chris","Jonathan","Lemur","Harry","Percy","Peregrine","James","Jamie","Sidney","Gabriel","Leyton","Curtley","Jarvis"]

isVowel :: Char -> Bool
isVowel c = (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u')

syllableCount :: String -> Int
syllableCount s
  | {- Override algo -} (s == "Anthony" || s == "Gabriel") = 3
  | {- Override algo -} (s == "James") = 1
  | (doSyllableCount s == 0) = 1
  | otherwise = doSyllableCount s

-- Try... data TabConfig = TabConfig { tabX, tabY, tabWidth, tabHeight :: Int, tabPosition :: Position, tabMargin :: Margin }

doSyllableCount :: String -> Int
doSyllableCount xs = let (currSyll, wasVowel, lastChar, idx, numChars) = foldl updateSyllsForChar (0, False, '.', 0, length xs) xs  -- FIXME!!!
  in currSyll
  where updateSyllsForChar (currSyll, wasVowel, lastChar, idx, numChars) c =
         if (wasVowel && ((lastChar == 'u' && c == 'a') || (lastChar == 'i' && c == 'a'))) then
           (currSyll + 1, wasVowel, c, idx + 1, numChars)
         else
           if (isVowel(c) || c == 'y') then
             if (wasVowel == False && ((c /= 'e') || idx < numChars - 1))
               then (currSyll + 1, True, c, idx + 1, numChars)
             else (currSyll, wasVowel, c, idx + 1, numChars)  -- No change here
           else (currSyll, False, c, idx + 1, numChars)

addIndexes :: (Int, String) -> String
addIndexes (idx,str) = (show idx) ++ " ... " ++ str

main = mapM_ (putStrLn . addIndexes) (zip [1..] [ i ++ " " ++ j ++ " Jordan-Regan" | i <- foreNames, j <- middleNames, i /= j, head i /= head j, compatible i j ]) where
  compatible s t =
    let s1 = syllableCount s; s2 = syllableCount t in not ((s1 == 1 && s2 == 1) || (s1 == 1 && s2 >= 3) || (s1 >= 3 && s2 >= 3) || (s1 >= 3 && s2 == 1))
