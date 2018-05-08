digit::Char->Bool
digit x = elem x ['0'..'9']

charOfDigit::(Num a, Show a, Ord a)=> a -> Maybe Char
charOfDigit x = if x < 10 && x >= 0 then Just (head . show $ x) else Nothing

among::(Eq t) => t -> [t] -> Bool
among x [] = False
among x (y:ys) 
			| x == y = True 
			| otherwise = among x ys

strcop::Int -> String -> String
strcop 0 xs = ""
strcop num xs = if num > 0 then xs ++ (strcop (pred num) xs) else "Error"


data Valor = As | Dos | Tre | Cuatro | Cinco | Seis | Siete | Sota | Caballo | Rey deriving (Show, Eq)
data Palo = Oros | Espadas | Bastos | Copas deriving (Show, Eq)
data Carta = Carta Valor Palo deriving (Show)

palode::Carta->Palo
palode (Carta _ x) = x
valorde::Carta->Valor
valorde (Carta x _) = x

espalo::Palo -> Carta -> Bool
espalo x (Carta _ y) = x == y
esvalor::Valor -> Carta -> Bool
esvalor x (Carta y _) = x == y

data Saco = Saco [(String, Int)] deriving (Show)

add::Saco -> String -> Saco
add (Saco s) x = let a = filter (\t1 -> x == fst t1) s in
                    if length(a) == 1 
 --                   then (Saco (fst (head a), snd (head a) + 1):(filter (\t1 -> x /= fst t1) s))
                    then (Saco ((fst (head a), 1 + snd (head a)):(filter (\t1 -> x /= fst t1) s)))
                    else (Saco ((x, 1):s))
                    
remove::Saco->(String, Saco)
remove (Saco []) = ("", Saco [])
remove (Saco (x:xs)) = if snd x == 1 
                        then (fst x, Saco xs)
                        else (fst x, (Saco ((fst x, (snd x) - 1):xs)))
                        
isempty::Saco->Bool
isempty (Saco s) | length s == 0 = True
                 | otherwise = False
                 
                 
uncurry'::(alpha->beta->gamma) -> ((alpha, beta) -> gamma)
uncurry' a = (\(x, y) -> a x y)

appR::[a] -> [a] -> [a]
appR x y = foldr (\t acc -> t:acc) y x 

mapR::(a -> b) -> [a] -> [b]
mapR f s = foldr (\t acc -> (f t):acc) [] s


