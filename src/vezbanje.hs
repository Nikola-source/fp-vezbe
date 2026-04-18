pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : (pack (dropWhile (== x) xs))

pack' :: (Eq a) => [a] ->[(a,Int)]
pack' [] = []
pack' xxs = [(head xs, length xs) | xs <- pack xxs]

data MyType a = Single a | Multi Int a

-- instance (Eq a) => Eq (MyType a) where
--     (Single x) == (Single y) = x == y
--     (Multi a x) == (Multi b y) = a == b && x == y
--     _==_ = False

instance (Show a) => Show (MyType a) where
    show (Single a) = "(" ++ show a ++ ")"
    show (Multi x b) = "(" ++ show x ++ " " ++ show b ++ ")"


pack'' :: (Eq a) => [a] -> [MyType a]
pack'' [] = []
pack'' xs = [if i > 1 then Multi i x else Single x|(x,i) <- pack' xs]

expand :: (MyType a) -> [a]
expand (Single x) = [x]
expand (Multi 0 x) = []
expand (Multi i x) = (x : expand (Multi (i-1) x))

decode :: [MyType a] -> [a]
decode [] = []
decode (x:xs) = (expand x) ++ decode xs


data Stablo st = Prazno | Stablo st (Stablo st) (Stablo st) deriving (Show)

dodaj :: (Ord a, Eq a) => a -> Stablo a -> Stablo a
dodaj x Prazno = Stablo x Prazno Prazno 
dodaj x (Stablo y levo desno)
    | x == y = Stablo y levo desno
    | x < y = Stablo y (dodaj x levo) desno
    | x > y = Stablo y levo (dodaj x desno)

nadji :: (Ord a) => a -> Stablo a -> Bool
nadji a Prazno = False
nadji a (Stablo x levo desno)
    | a == x = True
    | a < x = nadji a levo
    | a > x = nadji a desno

fromList :: (Ord a) => [a] -> Stablo a
fromList xs = foldr dodaj Prazno xs

toList :: (Ord a) => Stablo a -> [a]
toList Prazno = []
toList (Stablo x levo desno) = x : ((toList levo) ++ (toList desno))


data Broj = Jedan | Dva | Tri

rotiraj :: Broj -> (a, a, a) -> (a, a, a)
rotiraj Jedan (x, y, z) = (y, z, x)
rotiraj Dva (x, y, z) = (z, x, y)
rotiraj Tri (x, y, z) = (x, y, z) 

unazad :: String -> String
unazad "" = ""
unazad str = (unwords . map reverse . words) str

palindrom :: String -> Bool
palindrom str 
    | (removeSpaces str) == reverse (removeSpaces str) = True
    | otherwise = False 

removeSpaces :: String -> String
removeSpaces str = foldl (\s acc -> s ++ acc) "" (words str)
    
printLine :: String -> IO()
printLine str 
    | palindrom str = putStrLn "Palindrom"
    | otherwise = putStrLn (unazad str)

main :: IO()
main = do
    line <- getLine
    if null line then return()
    else printLine line
    main

data Imenik = Imenik [Osoba]
data Osoba = Osoba {ime :: String, telefon :: String, email :: String}

noMail :: Imenik -> [(String, String)]
noMail (Imenik []) = []
noMail (Imenik  ((Osoba ime telefon mail):xs)) = (ime, telefon) : noMail (Imenik xs)