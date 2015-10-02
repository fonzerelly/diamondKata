module Diamond where
import Data.List

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
defineDiamondLines c = concat [preC, [c], reverse preC]
   where preC = ['A' .. pred c]

createDiamondLine :: Int -> Char -> String
createDiamondLine 0 _ = ""
createDiamondLine maxLen 'A' = concat [(outer maxLen 'A'), "A"]
createDiamondLine maxLen c = concat [(outer maxLen c), [c], (inner c), [c]]

createDiamond :: Char -> String
createDiamond 'A' = "A"
createDiamond c = unlines $ map (createDiamondLine maxLen) (defineDiamondLines c)
   where maxLen = (length (inner c)) + 2

