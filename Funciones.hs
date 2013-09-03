getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (words contents)	
				   
identificadorOperadores:: String -> IO ()
identificadorOperadores x
		 if isInfixOf ">" x then do
			let f = "(Mayor que," ++ x ++ ")" ++ "\n"
			putStrLn f
		else if isInfixOf ":" x then do
			let f = "(Dos puntos, " ++ x ++ " )" ++ "\n"
			putStrLn f
		else if isInfixOf "<" x then do
			let f = "(Menor que, " ++ x ++ " )" ++ "\n"
			putStrLn f
		else if isSuffixOf "\\" x then do
			let f = "(Barra Invertida, " ++ x ++ " )" ++ "\n"
			putStrLn f
		else if isInfixOf "\\" x then do
			let f = "(Barra Invertida, " ++ x ++ " )" ++ "\n"
			putStrLn f
		else if isInfixOf "=" x then do
			let f = "(Asignacion, " ++ x ++ " )" ++ "\n"
			putStrLn f
		else if isInfixOf "||" x || isInfixOf "&&" x then do
			let f = "(Operador logico, " ++ x ++ " )" ++ "\n"
			putStrLn f
		else if isInfixOf "&" x then do
			let f = "(Referencia, " ++ x ++ " )" ++ "\n"
			putStrLn f