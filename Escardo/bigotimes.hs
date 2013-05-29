type J r x = (x -> r) -> x
type K r x = (x->r) -> r

-- with r = Bool, elements of K are existential/universal q

overline :: J r x -> K r x
overline e p = p(e p)
-- exists x.p is p(e p)  (this is very classical)
-- maybe should be <e p, p(e p)>
findBool :: J Bool Bool