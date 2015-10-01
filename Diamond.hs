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

defineDiamondLines :: Char -> [Char]
defineDiamondLines 'A' = "A"
defineDiamondLines c = preC ++ [c] ++ reverse preC
   where preC = ['A' .. pred c]



createDiamond :: Char -> String
createDiamond = undefined
-- createDiamond c = out
--    where out = map (diamondLine maxLen) ['A' .. c] ++ [(pred c) .. 'A']
--       where diamondLine maxLen curChar = (outer maxLen)
--          where
--    maxLen = (length $ inner c) + 2
--          wherre
-- createDiamond c = map dummy $  [c .. 'A']
--    where dummy currentChar = currentChar : whitespaces : currentChar

