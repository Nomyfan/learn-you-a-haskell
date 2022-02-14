import           Data.Char
import           System.IO (Handle, IOMode (ReadMode), hClose, hGetContents,
                            openFile)

-- main = do
--   putStrLn "What's your first name?"
--   firstName <- getLine
--   putStrLn "What's your last name?"
--   lastName <- getLine
--   let bigFirstName = map toUpper firstName
--       bigLastName = map toUpper lastName
--   putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- main = do
--   contents <- getContents
--   putStr (map toUpper contents)

-- main = interact $ unlines . filter ((<10) . length) .lines

-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle
-- ↓
main = do
  contents <- readFile "girlfriend.txt"
  putStr contents

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result


