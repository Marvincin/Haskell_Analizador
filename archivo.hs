separador [] = return ()
separador (x:xs) = do 
			let z = split (oneOf "(;<>{[+-=!#\"%&*)}]") x
			instancias z
			separador xs