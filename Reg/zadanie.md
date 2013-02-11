Dany (w pliku Reg.hs) typ danych reprezentujący wyrażenia regularne nad alfabetem c:

~~~~ {.haskell}
data Reg c
  = Lit c           -- jeden znak 
  | Reg c :> Reg c  -- konkatenacja
  | Reg c :| Reg c  -- alternatywa (suma)
  | Many (Reg c)    -- gwiazdka 
  | Eps             -- slowo puste
  | Empty           -- jezyk pusty
  deriving (Eq,Show)    
~~~~

Uzupełnij moduł RegExtra o definicje omówione poniżej (ewentualnie zastepując występujące w nim `undefined`)

## Upraszczanie

Wyrażenia regularne nazywamy *równoważnymi* jeśli akceptują one ten sam język.
Dla danego wyrażenia regularnego nierzadko możemy podać prostsze wyrażenie 
równoważ, np  `Eps :> (Lit 0 :| Empty)` jest równoważne `Lit 0`

Napisz funkcję

~~~~
simpl :: Eq c => Reg c -> Reg c
~~~~

dającą wyrażenie równoważne argumentowi a prostsze (w jakimś sensie).
Niektóre potrzebne uproszczenia ujawnią się w późniejszych etapach

## Monoid

Dana (Mon.hs) klasa reprezentującą monoidy

~~~~
class Mon m where
  m1 :: m
  (<>) :: m -> m -> m
  
-- ** Properties:
-- * leftUnit x = m1 <> x == x
-- * rightUnit x =  x <> m1 == x
-- * assoc x y z = (x<>y)<>z == x<>(y<>z)
~~~~
