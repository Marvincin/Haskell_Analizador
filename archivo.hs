separador [] = return ()
separador (x:xs) = do 
			let z = split (oneOf "(;<>{[+-=!#\"%&*)}]") x
			instancias z
			separador xs

identificadorPalabrasReservadas:: String -> IO ()
identificadorPalabrasReservadas x = do
				if isInfixOf "+" x || isInfixOf "-" x || isInfixOf "*" x || isInfixOf "/" x || isInfixOf "^" x then  do
					let f = "(Operacion, " ++ x ++ " )" ++ "\n"
					putStrLn f
				else if isInfixOf "main" x || isInfixOf "void" x || isInfixOf "int" x || isInfixOf "float" x || isInfixOf "double" x || isInfixOf "return" x || isInfixOf "if" x || isInfixOf "else" x || isInfixOf "while" x || isInfixOf "do" x || isInfixOf "char" x || isInfixOf "goto" x || isInfixOf "break" x || isInfixOf "case" x || isInfixOf "auto" x || isInfixOf "const" x || isInfixOf "continue" x || isInfixOf "default" x || isInfixOf "enum" x || isInfixOf "long" x || isInfixOf "register" x || isInfixOf "short" x || isInfixOf "signed" x || isInfixOf "sizeof" x || isInfixOf "static" x || isInfixOf "struct" x || isInfixOf "typedef" x || isInfixOf "unsigned" x || isInfixOf "switch" x then do
					let f = "(Palabra Reservada, " ++ x ++ ")" ++ "\n"
					putStrLn f
				else putStrLn "Nada"