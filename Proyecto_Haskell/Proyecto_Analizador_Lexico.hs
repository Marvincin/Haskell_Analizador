import System.IO
import Data.Char(toUpper)
import Data.List
import Data.List.Split

{- Función que me permite obtener la información de un archivo
Participantes en la función:
		Joao Sanga
-}
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)	
main = do 
	putStrLn "Analizador Lexico basado en C"
	putStrLn ""
	putStrLn ""
	putStrLn "Ingrese el nombre del archivo que desea analizar con su respectiva extencion"
	abrir <- getLine
	putStrLn "Ingrese el nombre del archivo donde desea que se guarde (Sin .txt)"
	guardar <- getLine
	putStrLn "**********Analizador Lexico**********"
	putStrLn "*************************************"
	putStrLn "*Integrantes:                       *"
	putStrLn "*             Marlon Loayza         *"
	putStrLn "*             Adrian Aguilar        *"
	putStrLn "*             Joao Sanga            *"
	putStrLn "*                                   *"
	putStrLn "*************************************"
	putStrLn ""
	putStrLn ("Leyendo el archivo: " ++ abrir)
	codigo <- getWords abrir
	separador (guardar++".txt") codigo
	putStrLn "Analisis Lexico culminado"
	putStrLn ("Se guardo en: " ++ guardar++".txt")

{-Función que me permite separar las cadenas a traves de los delimitadores	
Participantes en la función:
		Adrian Aguilar
-}
separador path [] = return ()
separador path (x:xs) = do 
			let z = split (oneOf "(;<>{[+-?=!#:\\\"%&*)}]") x
			analizadorLexico path z
			separador path xs

{-Funcion que me permite identificar todas los tokens lexemas
Participantes en las funciones:
	Adrian Aguilar
	Marlon Loayza
	Joao Sanga
-}
analizadorLexico path [] = return ()
analizadorLexico path (x:xs) = do
			if x == "" then return ()
			else if isInfixOf "#" x then  do
				let f = "(Simbolo especial," ++ x ++ ")" ++ "\n"
				appendFile path f			
			else if isInfixOf "!" x then do
				let f = "(Admiracion cerrado," ++ x ++ ")" ++ "\n"
				appendFile path f			
			else if isInfixOf ">" x then do
				let f = "(Mayor que," ++ x ++ ")" ++ "\n"
				appendFile path f			
			else if isInfixOf ":" x then do
				let f = "(Dos puntos, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "<" x then do
	 			let f = "(Menor que, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isSuffixOf "\\" x then do
				let f = "(Barra Invertida, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "\\" x then do
				let f = "(Barra Invertida, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "=" x then do
				let f = "(Asignacion, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "||" x || isInfixOf "&&" x then do
				let f = "(Operador logico, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "&" x then do
				let f = "(Referencia, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "," x then do
				let f = "(Coma, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "\"" x then do
				let f = "(Comillas, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "%" x then do
				let f = "(Porcentaje, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "+" x || isInfixOf "-" x || isInfixOf "*" x || isInfixOf "/" x || isInfixOf "^" x then  do
				let f = "(Operacion, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "main" x || isInfixOf "void" x || isInfixOf "int" x || isInfixOf "float" x || isInfixOf "double" x || isInfixOf "return" x || isInfixOf "if" x || isInfixOf "else" x || isInfixOf "while" x || isInfixOf "do" x || isInfixOf "char" x || isInfixOf "goto" x || isInfixOf "break" x || isInfixOf "case" x || isInfixOf "auto" x || isInfixOf "const" x || isInfixOf "continue" x || isInfixOf "default" x || isInfixOf "enum" x || isInfixOf "long" x || isInfixOf "register" x || isInfixOf "short" x || isInfixOf "signed" x || isInfixOf "sizeof" x || isInfixOf "static" x || isInfixOf "struct" x || isInfixOf "typedef" x || isInfixOf "unsigned" x || isInfixOf "switch" x then do
				let f = "(Palabra Reservada, " ++ x ++ ")" ++ "\n"
				appendFile path f			
			else if isInfixOf ";" x then do
				let f = "(Punto y coma, " ++ x ++ " )" ++ "\n"
				appendFile path f			
			else if isInfixOf "'" x then do
				let f = "(Comilla simple, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "{" x then do
				let f = "(Llave abierta, " ++ x ++ " )" ++ "\n"
				appendFile path f			
			else if	isInfixOf "}" x then do
				let f = "(llave cerrada, " ++ x ++ " )" ++ "\n"
				appendFile path f			
			else if isInfixOf "(" x then do
				let f = "(Parentesis abierto, " ++ x ++ " )" ++ "\n"
				appendFile path f			
			else if isInfixOf ")" x then do
				let f = "(Parentesis cerrado, " ++ x ++ " )" ++ "\n"
				appendFile path f			
			else if isInfixOf "[" x then do
				let f = "(Corchete abierto, " ++ x ++ ")" ++ "\n"
				appendFile path f			
			else if isInfixOf "?" x then do
				let f = "(Interrogacion cerrado, " ++ x ++ " )" ++ "\n"
				appendFile path f
			else if isInfixOf "]" x then do
				let f = "(Corchete cerrado, " ++ x ++ ")" ++ "\n"
				appendFile path f			
			else do 
				let f = "(Identficador, " ++ x ++ ")" ++ "\n"
				appendFile path f			
			analizadorLexico path xs