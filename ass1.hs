{-
    Junho Sohn
    301301147
    junos@sfu.ca

    CMPT 383
    Assignment 1 - Rainbow Table
-}
 

import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe


pwLength, nLetters, width, height :: Int
filename :: FilePath
filename = "table.txt"  -- filename to store the table

pwLength = 8         -- length of each password
nLetters = 5         -- number of letters to use in passwords: 5 -> a-e
width = 40            -- length of each chain in the table
height = 1000           -- number of "rows" in the table

-- pwLength = 5
-- nLetters = 18
-- width = 60
-- height = 800







-- mod & div
mod1 :: Hash -> [Int]
mod1 x = [fromEnum x `mod` nLetters]
div1 :: Hash -> Int
div1 x = fromEnum x `div` nLetters

-- Create list & convert to string
createList :: Hash -> [Int]
createList x = mod1 x ++ createList (recurse)
    where recurse = toEnum(div1 x)::Hash

listToPass :: [Int] -> String
listToPass [] = []
listToPass (x:xs) = [toLetter x] ++ listToPass xs

-- Reduce from Hash -> Passwd
pwReduce :: Hash -> Passwd
pwReduce hash = listToPass $ reverse (take pwLength (createList hash))




-- Building rainbow table
performHash :: Int -> [Passwd] -> [Hash]
performHash 0 passwd = map pwHash passwd
performHash w passwd = performHash (w-1) (map pwReduce $ map pwHash passwd)

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable w passwd = Map.fromList (zip (performHash w passwd) passwd)


generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename


-- test1 :: IO (Maybe Passwd)
-- test1 = do
--   table <- readTable filename
--   return (Map.lookup 2017930117 table)





-- Reversing Hashes
findPasswordTuple :: (Hash, Passwd) -> Int -> Hash -> (Hash, Passwd)
findPasswordTuple tuple w hash
    | pwHash (snd tuple) == hash || w == 0    = tuple
    | otherwise = findPasswordTuple newTuple (w-1) hash
        where newTuple = ( (fst tuple), (pwReduce $ pwHash (snd tuple)) )

findPwd :: [(Hash, Passwd)] -> Int -> Hash -> Maybe Passwd
findPwd (x:xs) w hash
    | length xs == 0            = Nothing
    | pwHash foundPwd == hash   = Just foundPwd
    | otherwise                 = findPwd xs w hash
        where foundPwd = snd (findPasswordTuple x w hash)

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table w hash
  | hash == (0::Hash)   = Nothing
  | otherwise           = findPwd (Map.toList (table)) w hash 





test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Data.Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)


main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res


{-
ghc -O2 --make -Wall ass1.hs
./ass1

outputs: 
738 = 7.38%
749 = 7.49%
757 = 7.57%
-}