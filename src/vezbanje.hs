palindromLista :: (Eq a) => [a] -> Bool
palindromLista [] = True
palindromLista xs 
    | xs == (reverse xs) = True
    | otherwise = False