replicate' :: Int -> Int -> [Int]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x

repete :: Int -> [[Int]]
repete 0 = []
repete num = (replicate' num num) : repete (num - 1)
