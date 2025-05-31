fibonacci :: Int -> [Int]
fibonacci 0 = []
fibonacci 1 = [0]
fibonacci 2 = [0, 1]
fibonacci n = aux (n-2) 0 1 [0,1] 

aux :: Int -> Int -> Int -> [Int] -> [Int]
aux 0 a b lista = lista
aux contagem a b lista = aux (contagem - 1) b (a+b) (lista ++ [a+b]) 
