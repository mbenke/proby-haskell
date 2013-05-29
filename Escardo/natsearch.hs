
type N = Integer
enumerate :: ((N -> Bool) -> N) ->[N]
enumerate find = [n | n <- [0..m], forsome(\x -> x ==  n)]
  where
        forsome p = p(find p)
        forevery p = not(forsome(\a -> not(p a)))
        m = find(\x -> forevery(\n -> n <= x))

find' :: (N->Bool)->N
find' p = if p 17 then 17 else (if p 777 then 777 else 13)
 