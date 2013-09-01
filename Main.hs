module Main where
import System.IO
import Data.Char(toUpper)
import System.Directory
import Data.List
import Data.List.Split



main::IO()
main = undefined

factorial :: Integer -> Integer 
factorial n = product [1..n]

factorial' :: Integer -> Integer
factorial' 0 = 1 
factorial' n = n * factorial' ( n - 1 )


factorialesHastaN :: Integer -> [(Integer,Integer)]
factorialesHastaN n = [ (x,factorial x) | x<- [1..n] ]

--fibonacci :: Integer -> Integer
--fibonacci 0 = 1
--fibonacci 1 = 1
--fibonacci c = fibonacci (c-1) + fibonacci(c-2)

fibonacci :: Int->Int
fibonacci n = fibonacci' [1,1] n 

fibonacci' :: [Int]->Int->Int
fibonacci' x n 
        | n == (length x) = head x 
fibonacci' all@(ultimo:penultimo:xs) n = fibonacci' ([ultimo + penultimo]++all) n

mostrarnumeros::[Int]->Float
mostrarnumeros y = fromIntegral(sum y) / fromIntegral(length y)

esprimo:: Int->Bool
esprimo 1 = True
esprimo n = do
		let lista = [2..n-1]
		let y = esprimo' (n,lista)
		if null(lista) then True
			else if(y==1) then True 
				else False
		
esprimo':: (Int,[Int])->Int
esprimo' (x,lista) = do
		let e = head(lista)
		let z = mod x e
		if (null(lista)) then 1 
			else if (z == 0 ) then 0
				else esprimo' (x,tail(lista))
		


analizador:: [String] -> IO ()
analizador [x] = do
	if isInfixOf "#" x then  do 
	let z = splitOn "#" x
	putStrLn "numeral #"
	analizador (tail z)
	else if isInfixOf "include" x then  do 
	let z = splitOn "include" x
	putStrLn "Palabra reservada include"
	analizador (tail z)
	else if isInfixOf "<" x then  do 
	let z = splitOn "<" x
	putStrLn "Menor que <"
	analizador (tail z)
	else if isInfixOf ">" x then  do 
	putStrLn "Meyor que >"
	analizador (tail z)
	else putStr "No hay mas"