palindromo :: [Int] -> Bool
palindromo lista | lista == reverse lista = True
                 | otherwise = False
