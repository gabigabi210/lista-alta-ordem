-- exercício 2
busca :: [a] -> Int -> a
busca (x:xs) n | n == 0 = x
               | otherwise = busca xs (n-1)

posicao :: ([a] -> Int -> a) -> [a] -> Int -> a
posicao func lista n = func lista n

main :: IO()
main = do
    let a = (+1)
    let b = 90
    print (posicao busca [9,2,3,4] 2 )
