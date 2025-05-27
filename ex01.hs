
par :: Int -> Bool
par num | num `mod` 2 == 0 = True
        | otherwise = False

ordena :: [Int] -> [Int]
ordena [] = []
ordena [x] = [x]
ordena (x:y:xs) | x > y = y : ordena (x:xs)
                | otherwise = x : ordena (y:xs)

ordenaCompleta :: [Int] -> [Int]
ordenaCompleta xs |xs == xs' = xs
                  |otherwise = ordenaCompleta xs'
                    where
                        xs' = ordena xs

ordenadaPares ::([Int] -> [Int]) -> (Int -> Bool) -> [Int] -> [Int]
ordenadaPares funca funcb lista = [x | x <- funca (lista), funcb x]

main :: IO()
main = do
    let a = (+1)
    let b = 90
    print(ordenadaPares ordenaCompleta par [9,2,3,4])
