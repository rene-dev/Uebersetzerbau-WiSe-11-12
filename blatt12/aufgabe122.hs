{-
Aufgabe 12.2 
Gruppe: Dienstags 16-17 Uhr

Christoph Stahl 116531
Rene Hopf 131247
Sabrina Friesenborn 127532
-}

import Data.List

-- Eine Grammatik ist ein 3er-Tupel aus Nichtterminalsymbolen, Terminalsymbolen und Regeln
-- Terminalsymbole und Nichtterminalsymbole sind hier einfache Zeichen
-- Regeln sind Zweiertupel mit einem Nichtterminalsymbol, und einer rechten Regelseite
type Grammar = ([Char],[Char],[(Char, String)])

getNonterminals (n,_,_) = n
getTerminals (_,t,_) = t
getRules (_,_,r) = r

example :: Grammar
example = (['S','A','B'],['a','b'],[('S',"aB"),('S',"bA"),('S',"ε"),('A',"aS"),('A',"bAA"),('B', "bS"),('B', "aBB")])

-- ableitungen gibt zu einer gegebenen Grammatik und einem gegebenen Nichtterminalsymbol eine Liste an möglichen ableitungen (ersten Grades) zurück
ableitungen :: Grammar -> Char -> [String]
ableitungen (_,_,r) x = map snd $ filter (\(l,_) -> l == x) r

-- ersetze gibt zu einer gegebenen Grammatik einem gegebenen Wort (beginnend mit einem Nichtterminalsymbol) alle Wörter zurück, in denen der erste Buchstabe (Also das Nichtterminalsymbol) mittels der Funktion "ableitung" ersetzt wurde.
ersetze :: Grammar -> String -> [String]
ersetze g xs = map (flip (union) $ (tail xs)) $ ableitungen g $ head xs

-- first berechnet zu einer gegebenen Grammatik und einem gegebenen Wort die Firstmengen.
first :: Grammar -> String -> [String]
first g [] = []  -- Leeres Wort -> Leere Firstmenge
first g xs@(x:w)
        | x `elem` (getTerminals g) = [[x]] -- Startet mit einem Terminalsymbol -> Menge mit dem Terminalsymbol
	| x == 'ε' = first g w -- Startet mit "ε" (angenehm, dass Haskell so einfach utf8 unterstützt) -> mach das Ganze ohne Epsilon
	| x `elem` (getNonterminals g) = foldl (union) [] $ map (first g) $ ersetze g xs x -- Startet mit einem Nichtterminalsymbol -> ersetze Nichtterminalsymbol mithilfe der Ableiteregeln, und wende first auf alle sich so ergebenden Worte an
	| otherwise = []
