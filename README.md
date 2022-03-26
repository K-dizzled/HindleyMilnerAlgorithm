# Hindley-Milner-algorithm
Implementation of the Hindley-Milner type inference algorithm.

## Usage
```haskell
GHCi> term = Lam "y" $ Var "x"
GHCi> env = Env [("x",TVar "a" :-> TVar "b")]
GHCi> let Right eqs = equations env term (TVar "o") in eqs
[(TVar "d",TVar "a" :-> TVar "b"),(TVar "c" :-> TVar "d",TVar "o")]
GHCi> let Left err = equations (Env []) term (TVar "o") in err
"There is no variable \"x\" in the enviroment."

GHCi> let Right pp = principlePair (Var "x") in pp
(Env [("x",TVar "a")],TVar "a")
GHCi> let Right pp = principlePair (Var "f" :@ Var "x") in pp
(Env [("f",TVar "a" :-> TVar "b"),("x",TVar "a")],TVar "b")
GHCi> let Right pp = principlePair (Lam "x" $ Lam "y" $ Var "y") in pp
(Env [],TVar "a" :-> (TVar "b" :-> TVar "b"))
GHCi> let Left err = principlePair (Var "x" :@ Var "x") in err
"Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"b\")!"
```
