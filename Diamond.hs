module Diamond where
import Data.Char

inner :: Char -> String
inner c
   | c == 'A' = ""
   | c == 'B' = " "
   | otherwise = replicate ((length $ inner $ pred c) + 2) ' '

outer :: Int -> Char -> String
outer maxLen 'A' = replicate ((maxLen - 1) `div` 2) ' '
outer maxLen c   = replicate ((maxLen - ((length (inner c)) + 2)) `div` 2) ' '

createDiamond :: Char -> String
createDiamond = undefined
-- createDiamond c = map dummy $  [c .. 'A']
--    where dummy currentChar = currentChar : whitespaces : currentChar

