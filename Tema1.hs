module Tema1 (
        solveSimple,
        solveCosts
        ) where

import Data.Array

-- a trebuit sa imi fac un tip de date nou pentru ca practic Nothing era
-- -Infinit iar mie imi trebuia Infinit

data Numar = Infinit | Numar Int

instance Eq Numar where
    (==) Infinit Infinit = True
    (==) (Numar x) (Numar y) | x == y = True
                             | otherwise = False
    (==) _ _ = False

instance Ord Numar where
    (<=) Infinit (Numar x) = False
    (<=) (Numar x) (Numar y) = x <= y
    (<=) Infinit Infinit = True
    (<=) (Numar x) Infinit = True

instance Num Numar where

    (+) Infinit Infinit = Infinit
    (+) (Numar x) (Numar y) = Numar (x + y)
    (+) Infinit (Numar x) = Infinit
    (+) (Numar x) Infinit = Infinit
   -- Nu am nevoie de mai mult

 -- nr_orase, [(nod1, nod2,distanta)] -> Maybe [lista_orase],nr_orase traseu
solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)

solveSimple x = let
    
 -- un Int de aici e redundant dar nu il modific ca ar fi niste bataie de cap
                inf_graph :: [Int -> [(Int, Int, Int)]]
                inf_graph = (get_way (snd x)):inf_graph

                nats :: [Int]
                nats = 1:(map (+1) nats)

                repeat :: a -> [a]
                repeat x = x:(repeat x)
                    
                liste_adiacenta = listArray (1, fst x) (zipWith (\a -> \b -> a b) inf_graph nats)

                get_way :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
            
                get_way (x:xs) n = (filter ( \(a, b, c) -> (a == n) || (b == n)) (x:xs))
                get_way [] n = []

                get_cost :: Int -> Int -> Int
                get_cost n m = case filter (\(a, b, c) -> (a == m) || (b == m)) (liste_adiacenta ! n) of
                                 ((a, b, c):xs) -> c

-------------------------------------------------------------------------------

-- Bellman

                best_value_list = best_value:(best_value_list)

                bellman :: Int -> Int -> (Numar, Numar)
                bellman 0 y | y == (fst x) = ((Numar 0), Infinit)
                            | otherwise = (Infinit, Infinit)

                bellman z y = min (best_value ! ((z-1), y)) (foldr (\l -> \m -> min l m) (Infinit, Infinit) 
                 (zipWith (\j -> \(p, r) -> (fst (j ! (p, r)) + (Numar (get_cost r y)), (Numar r))) (take (length (liste_adiacenta ! y)) best_value_list)
                 (zipWith (\f -> \g -> (f, g)) (take (length (liste_adiacenta ! y)) (repeat (z-1))) (map (\(a, b, c) -> if a /= y then a else b) (liste_adiacenta ! y)))))

                bounds = ((0, 1), (((fst x) - 1) ,(fst x)))

                best_value = listArray bounds [bellman n m | (n, m) <- range bounds]



                parcurgere :: Numar -> Int -> [Int]

                
                parcurgere (Numar y) z = case (snd (best_value ! (z, y))) of
                                        (Numar x) -> x:(parcurgere (Numar x) (z-1))
                                        Infinit -> []

                in if fst (best_value ! (((fst x) -1), 1)) /= Infinit
                then
                    Just ((1:(parcurgere (Numar 1) ((fst x) -1))), (\(Numar p) -> p) (fst (best_value ! (((fst x) -1), 1))))
                else
                    Nothing

solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)




solveCosts tuplu = let

            get_muchii :: (Int, Int, [Int], [(Int, Int, Int)]) -> [(Int, Int, Int)]
            get_muchii (_, _, _, x) = x

            get_nr_orase :: (Int, Int, [Int], [(Int, Int, Int)]) -> Int
            get_nr_orase (x, _, _, _) = x

            get_bani :: (Int, Int, [Int], [(Int, Int, Int)]) -> Int
            get_bani (_, x, _, _) = x

            get_costs :: (Int, Int, [Int], [(Int, Int, Int)]) -> [Int]
            get_costs (_, _, x, _) = x

            get_cost_oras :: Int -> Int
            get_cost_oras n = head (drop (n-1) (get_costs tuplu))

            nats :: [Int]
            nats = 1:(map (+1) nats)

            repeat :: a -> [a]
            repeat x = x:(repeat x)

            liste_adiacenta = listArray (1, (get_nr_orase tuplu)) (zipWith (\a -> \b -> a b) (repeat (get_way (get_muchii tuplu))) nats)

            get_way :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]

            get_way [] n = []
            get_way (x:xs) n = filter (\(a, b, c) -> (a == n) || (b == n)) (x:xs)

            get_length :: Int -> Int -> Int
            get_length n m = case filter (\(a, b, c) -> (a == m) || (b == m)) (liste_adiacenta ! n) of
                            ((a,b,c):xs) -> c

            
            --            muchii | nod | bani | dist | bani | drum
            mod_bellman :: Int -> Int -> Int -> (Numar, Numar, Numar)

            mod_bellman 0 x z | x == (get_nr_orase tuplu) = ((Numar 0), (Numar 0), Infinit)
                              | otherwise = (Infinit, Infinit, Infinit)
{-
            mod_bellman _ x 0 | x == (get_nr_orase tuplu) = ((Numar 0), (Numar 0), Infinit)
                              | otherwise = (Infinit, (Numar 0), Infinit)
-}
{-}
            mod_bellman x y z = min (best_value ! ((x-1), y, z)) (foldl (\(a, b, c) -> \(d, e, f) -> if e <= (Numar z) then min (a, b, c) (d, e, f) else (a, b, c)) (Infinit, Infinit, Infinit)
                                 (zipWith (\a -> \(b, c, d) -> ( ( (\(e, f, g) -> e + (Numar (get_length y c))) (a ! (b, c, d))), (\(e, f, g) -> f + (Numar (get_cost_oras c))) (a ! (b, c, d)) , (Numar c)) )
                                  (repeat best_value) (zipWith (\a -> \(b, c) -> (a, b, c)) (repeat (x-1)) ( foldl (\h -> \k -> h ++ k) []  
                                   (zipWith (\a -> \b -> (zipWith (\c -> \d -> (c, d)) a (repeat b) )) (take (z+1) (repeat (map (\(r, t, u) -> if r /= y then r else t) (liste_adiacenta ! y) ))) (take (z+1) (0:nats)) )) )))
-}
{-}
            mod_bellman x y z = min (best_value ! ((x-1), y, z)) (foldl (\(a, b, c) -> \(d, e, f) -> if e <= (Numar z) then min (a, b, c) (d, e, f) else (a, b, c)) (Infinit, Infinit, Infinit)
                                 (zipWith (\l -> \(k, h, g) -> ( ( (\(e, f, g) -> e + (Numar (get_length y h))) (l ! (k, h, g))), (\(e, f, g) -> f + (Numar (get_cost_oras h))) (l ! (k, h, g)) , (Numar h)) )
                                  (repeat best_value) (zipWith (\aa -> \(bb, cc) -> (aa, bb, cc)) (repeat (x-1)) ( foldl (\h -> \k -> h ++ k) []  
                                   (zipWith (\q -> \w -> map (\v -> (v, w)) q) (take (z) (repeat (map (\(r, t, u) -> if r /= y then r else t) (liste_adiacenta ! y) ))) (take (z) (0:nats)) )) )))
-}
            mod_bellman x y z = (min (best_value ! ((x-1), y, z)) (foldl (\(a, b, c) -> \(d, e, f) -> if e <= (Numar z) then min (a, b, c) (d, e, f) else (a, b, c)) (Infinit, Infinit, Infinit)
                                 (zipWith (\l -> \(k, h, g) -> ( ( (\(e, f, g) -> e + (Numar (get_length y h))) (l ! (k, h, g))), (\(e, f, g) -> f + (Numar (get_cost_oras h))) (l ! (k, h, g)) , (Numar h)) )
                                  (repeat best_value) (filter (\(_, _, m) -> m >= 0) (zipWith (\aa -> \bb -> (aa, bb, (z - (get_cost_oras bb) ))) (repeat (x-1)) (map (\(r, t, u) -> if r /= y then r else t) (liste_adiacenta ! y) ))))  ))



                                --(filter (\(a, b, c) -> ((a-1) == x) && (liste_adiacenta ! y) (range bounds)) 

            bounds = ((0, 1, 0), (((get_nr_orase tuplu) -1), get_nr_orase tuplu, get_bani tuplu))

            best_value = listArray bounds [mod_bellman x y z | (x, y, z) <- range bounds]

                       -- oras | muchii | bani
            parcurgere :: Numar -> Int -> Int -> [(Int, Int)]

            parcurgere (Numar x) y z = case best_value ! (y, x, z) of
                                        (a, (Numar b), (Numar c)) -> (c, z - (get_cost_oras c)):(parcurgere (Numar c) (y-1) (z - (get_cost_oras c)))
                                        (_, _, Infinit) -> []

            in if ((\(a, b, c) -> a /= Infinit) (best_value ! (((get_nr_orase tuplu) -1), 1, (get_bani tuplu))))
                then
                    Just ((1, get_bani tuplu):(parcurgere (Numar 1) ((get_nr_orase tuplu) -1) (get_bani tuplu)), (\((Numar a), b, c) -> a) (best_value ! ((get_nr_orase tuplu) -1, 1, get_bani tuplu)) )
                else
                    Nothing