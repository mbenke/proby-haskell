import qualified Data.Map

type Map k v = Data.Map.Map k v
find :: (Ord k) => k -> Map k v -> v
find = Data.Map.findWithDefault (error "symbol not found")
insert= Data.Map.insert

type Name = String

data Exp = Var Name | Lam Name Exp | App Exp Exp deriving Show
type Env = Map Name Val
data Val = Val Exp Env deriving Show

emptyEnv = Data.Map.empty

eval :: Exp -> Env -> Val
eval (Var n) env = find n env 
eval (App e1 e2) env = apply (eval e1 env) (eval e2 env)
eval e@(Lam _ _) env = Val e env
apply :: Val -> Val -> Val
apply (Val (Lam n b) env) e2 = eval b (insert n e2 env) 
