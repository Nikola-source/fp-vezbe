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

data Stablo a = Prazno | Stablo a (Stablo a) (Stablo a) deriving (Show)

sadrzi :: (Ord a) => a -> Stablo a -> Bool
sadrzi a Prazno = False
sadrzi a (Stablo vr levo desno)
    | a == vr = True
    | a < vr = sadrzi a levo
    |a > vr = sadrzi a desno

dodaj :: (Ord a) => a -> Stablo a -> Stablo a
dodaj x Prazno = Stablo x Prazno Prazno
dodaj x (Stablo y levo desno)
        | x == y = Stablo y levo desno
        | x < y = Stablo y (dodaj x levo) desno
        | x > y = Stablo y levo (dodaj x desno)

velicina :: Stablo a -> Int
velicina Prazno = 0
velicina (Stablo a levo desno) = 1 + velicina levo + velicina desno

fromList :: (Ord a) => [a] -> Stablo a
fromList xs = foldr dodaj Prazno xs

dubina :: Stablo a -> Int
dubina Prazno = 0
dubina (Stablo a levo desno) = 1 + max (dubina levo) (dubina desno)

data Osoba = Osoba { ime :: String, prezime :: String, godiste :: Int } deriving (Show)
data Imenik = Imenik [Osoba]

starijiOd :: Int -> Imenik -> [String]
starijiOd _ (Imenik []) = []
starijiOd a (Imenik (x:xs)) 
    | (godiste x) < a = [(ime x) ++ " " ++ (prezime x)] ++ starijiOd a (Imenik xs)
    | otherwise = starijiOd a (Imenik xs)


poImenu :: String -> Imenik -> Maybe Osoba
poImenu _ (Imenik []) = Nothing
poImenu name (Imenik (x:xs))
    | (ime x) == name = Just x
    | otherwise = poImenu name (Imenik xs)

brReci :: String -> Int
brReci st = length (words (st))

main :: IO()
main = do 
    line <- getLine
    if line == "kraj" then (putStrLn "Dovidjenja")
        else do let br = brReci line
                putStrLn (show br)
                main

decode :: [(Int,a)] -> [a]
decode [] = []
decode ((br,a):xs) = (replicate br a) ++ decode xs

fil :: (Eq a) => [a] -> [a]
fil [] = []
fil (x:xs) = x : fil (filter (/= x) xs)

razliciti :: (Eq a) => [a] -> Int
razliciti xs = length (fil xs)

vrednostUMapi :: (Eq a) => a -> [(a,Int)] -> Maybe Int
vrednostUMapi _ [] = Nothing
vrednostUMapi a ((kljuc,vr):xs) 
    | a == kljuc = Just vr
    | otherwise = vrednostUMapi a xs

zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' f [] _ _ = []
zipWith3' f _ [] _ = []
zipWith3' f _ _ [] = []
zipWith3' f (x:xs) (y:ys) (z:zs) = (f x y z) : (zipWith3' f xs ys zs)