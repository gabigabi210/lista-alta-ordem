
--a)
par :: Int -> Bool
par n | n `mod` 2 == 0 = True
      | otherwise = False

all' :: (a -> Bool) -> [a] -> Bool
all' func [] = True
all' func (h:t) | func h == False = False
                | otherwise = all' func t
--b)
any' :: (a -> Bool) -> [a] -> Bool
any' func [] = False
any' func (h:t) | func h == True = True
                | otherwise = any' func t
--c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' func [] = []
takeWhile' func (h:t) | func h == False = []
                      | otherwise = h : takeWhile' func t
--d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' func [] = []
dropWhile' func (h:t) | func h == False = (h:t)
                      | otherwise = dropWhile' func t
