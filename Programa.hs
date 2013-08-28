import System.IO
import System.Directory
import Data.List.Split

main = do
    handle <- openFile "archivo.txt" ReadMode
    contents <- hGetContents handle
    let todoTasks = lines contents
    putStrLn "These are your TO-DO items:"
    hClose handle

esmayor :: String -> [String]
esmayor n = splitOn " " n
esmayor let n = take 1 n