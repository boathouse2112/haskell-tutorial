removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!!!"
lucky x = "Not a lucky number :("

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "can't do that, dumbass."
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty String, dumbass."
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, dumbass."
  | bmi <= normal = "You say you're normal, dumbass."
  | bmi <= fat    = "You're fat, dumbass."
  | otherwise     = "You're a land whale, dumbass."
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f : _) = firstname
        (l : _) = lastname

ifBlue :: String -> String
ifBlue x = if x == "blue" then "yes" else "nope"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n _
  | n <= 0 = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = (reverse xs) ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs)
  | a == x    = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = smallerSorted ++ [x] ++ biggerSorted
  where smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
