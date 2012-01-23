{-
Aufgabe 12.2 
Gruppe: Dienstags 16-17 Uhr

Christoph Stahl 116531
Rene Hopf 131247
Sabrina Friesenborn 127532
-}


import System
import System.CPUTime

-- Naiver Ansatz
c :: Int -> Int
c 0 = 1
c n = sum [(c k) * (c (n-k-1))|k <-[0..n-1]]


-- Optimierter Ansatz
-- Es wird eine unendliche Liste mit den Ergebnissen für alle Zahlen angelegt. Da Haskell Lazy ist, wird nur der Teil berechnet, der wirlich wichtig ist. 
-- Bei Aufruf von c' n wird einfach das n-te Element der Liste zurück gegeben. Daraufhin werden c' 0 bis c' 17 berechnet. Jedoch im Gegensatz zum naiven Ansatz nur ein mal.
--
-- Das Verfahren ist deutlich schneller:
-- [04:21:43/DS1]christoph blatt12$ ghc -O2 aufgabe121.hs -o catalan
-- [1 of 1] Compiling Main             ( aufgabe121.hs, aufgabe121.o )
-- Linking catalan ...
-- [04:21:46/DS1]christoph blatt12$ ./catalan n 20
-- Normal: C_20: 6564120420
-- Computation time: 51.462177s
-- [04:22:51/DS1]christoph blatt12$ ./catalan o 20
-- Optimized: C_20: 6564120420
-- Computation time: 3.0e-3s

c' :: Int -> Int
c' n = cs !! n

cs :: [Int]
cs = 1 : map (computeC' cs 0) [0..]

computeC :: [Int] -> Int -> Int
computeC xs j = computeC' xs 0 (j-1)

computeC' :: [Int] -> Int -> Int -> Int
computeC' xs i 0 = ((xs !! i) * (xs !! 0))
computeC' xs i j = ((xs !! i) * (xs !! j)) + (computeC' xs (i+1) (j-1))

-- Aufruf mit <Programmname> [n|N|o|O] <Int>
main = do
	args <- getArgs
	start <- getCPUTime
	if head args == "n" || head args == "N" then
		putStrLn $ "Normal: C_" ++ (head $ tail args) ++ ": " ++ (show $ c $ read $ head $ tail args)
	else if head args == "o" || head args == "O" then
			putStrLn $ "Optimized: C_" ++ (head $ tail args) ++ ": " ++ (show $ c' $ read $ head $ tail args)
		else do
			progName <- getProgName
			fail $ "Usage: " ++ progName ++ " [O|o|N|n] <Int>"
	end <- getCPUTime
	putStrLn $ "Computation time: " ++  show ((fromIntegral (end - start))/(10^12)) ++ "s"



