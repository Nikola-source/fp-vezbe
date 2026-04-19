palindromLista :: (Eq a) => [a] -> Bool
palindromLista [] = True
palindromLista xs 
    | xs == (reverse xs) = True
    | otherwise = False

duzina :: Int -> Int
duzina 0 = 0
duzina br = 1 + duzina (div br 10)


cifre :: Int -> [Int]
cifre 0 = []
cifre br = (cifre (div br 10)) ++ [mod br 10] 

armstrong :: Int -> Bool
armstrong br = (foldl1 (\acc s -> acc + s) (map (\a -> a^(length (cifre br))) (cifre br))) == br

parneNeparne :: [a] -> ([a],[a])
parneNeparne xs = pomocna 0 xs
    where 
        pomocna _ [] = ([],[])
        pomocna i (x:xs) 
            | odd i = (x:neparni, parni)
            | even i = (neparni, x:parni)
            where
                (neparni, parni) = pomocna (i+1) xs

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = Just (a/b)

mapIndex :: (Int -> a -> b) -> [a] ->[b]
mapIndex f xs = pomocna 0 f xs
    where
        pomocna _ _ [] = []
        pomocna i f (x:xs) = (f i x) : pomocna (i+1) f xs
