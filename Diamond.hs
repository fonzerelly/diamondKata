module Diamond where
import Data.Char

inner :: Char -> String
inner c
   | c == 'A' = ""
   | c == 'B' = " "
   | otherwise = replicate ((length $ inner $ pred c) + 2) ' '

createDiamond :: Char -> String
createDiamond = undefined
-- createDiamond c = map dummy $  [c .. 'A']
--    where dummy currentChar = currentChar : whitespaces : currentChar

