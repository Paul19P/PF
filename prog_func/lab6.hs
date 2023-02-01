import Distribution.Simple.Utils (xargs)
data Fruct = Mar String Bool | Portocala String Int
ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,Portocala "Sanguinello" 10,Portocala "Valencia" 22,Mar "Golden Delicious" True,Portocala "Sanguinello" 15,Portocala "Moro" 12,Portocala "Tarocco" 3,Portocala "Moro" 12,Portocala "Valencia" 2,Mar "Golden Delicious" False,Mar "Golden" False,Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala soi _) = if (soi=="Tarocco" || soi=="Moro" || soi=="Sanguinello") then
                                            True
                                        else
                                            False

nrFeliiPortocala :: Fruct -> Int
nrFeliiPortocala (Portocala _ felii) = felii

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x : l)
    | ePortocalaDeSicilia(x) == True = nrFeliiPortocala(x) + nrFeliiSicilia l
    | ePortocalaDeSicilia(x) == False  = 0 + nrFeliiSicilia l
    | otherwise = 0 + nrFeliiSicilia l

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi ((Portocala _ _) : l) = nrMereViermi l
nrMereViermi ((Mar _ x) : l)
    | x==True = 1+ nrMereViermi l
    | otherwise = 0+nrMereViermi l

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _)="Meow!"
vorbeste (Caine _ _)="Woof!"

rasa :: Animal -> Maybe String
rasa (Pisica _)= Nothing
rasa (Caine _ rasa)= Just rasa

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M m) n = foldr (\(L linie) l -> l && (foldr (+) 0 linie)==n) True m


verifLinie :: Linie -> Int -> Bool
verifLinie (L lin) n =
    if length lin ==n 
        then length (filter (>0) lin) ==n 
        else True
   


doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) n = foldr (\(L line) result -> result && verifLinie (L line) n) True m

corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M (x:y:line))=
    if (length first) == (length second) then
        True && corect (M (y:line))
    else
        False
    where
        first = (\(L x)->x) x
        second = (\(L y)-> y) y
