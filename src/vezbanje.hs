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
