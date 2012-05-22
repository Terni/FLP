---------- (L) Napsal PITEL
----------------------------------
data Lambda a
	    = LVar a -- V
	    | LApp (Lambda a) (Lambda a) -- (E1 E2)
	    | LAbs a (Lambda a) -- (\V . E)
	    deriving (Show, Eq)
-- \xyz.xa == LAbs "x" (LAbs "y" (LAbs "z" (LApp(LVar "x") (LVar "a"))))

-- Vrátí seznam všech volných proměnných v lambda výrazu.
freeVars :: Eq a => Lambda a -> [a]
freeVars (LVar v) = [v] -- Samotna promenna je proste vzdy volna
freeVars (LApp e1 e2) = (freeVars e1) ++ (freeVars e2)
freeVars (LAbs v e) = filter (/= v) $ freeVars e
-- pouziti: freeVars (LAbs "x" (LAbs "y" (LAbs "z" (LApp(LVar "x") (LVar "a")))))
-- pouziti: freeVars (LAbs "z" (LApp (LApp (LVar "a") (LVar "b")) (LVar "z")))

-- Vrátí seznam všech vázaných proměnných v lambda výrazu.
boundVars :: Eq a => Lambda a -> [a]
boundVars (LVar v) = [] --Promenna je volna!
boundVars (LApp e1 e2) = (boundVars e1) ++ (boundVars e2)
boundVars (LAbs v e) = (boundVars e) ++ filter (v ==) (freeVars e) -- Kdyz neni volna, tak je vazana
-- pouziti: boundVars (LAbs "x" (LAbs "y" (LAbs "z" (LApp(LVar "x") (LVar "a")))))

-- Proměnné které na sebe mohou vázat
boundingVars :: Lambda a -> [a]
boundingVars (LVar v) = [] --Promenna je volna!
boundingVars (LApp e1 e2) = (boundingVars e1) ++ (boundingVars e2)
boundingVars (LAbs v e) = v : boundingVars e
-- \xyz.xa == [xyz]

-- Alfa konverze (prejmenovani vazanych promenych)
alpha :: Eq a => Lambda a -> Lambda a -> Lambda a -> Lambda a
alpha e (LVar v) (LVar v') = if check then doAlpha e 
				      else error "Bounding problem!" 
  where
  check = (elem v (boundVars e)) && (not (elem v' ((freeVars e) ++ (boundingVars e)))) -- Nahrazovana promenna musi byt vazana a nahrazujici promenna nesmi byt volna nebo vazajici
  doAlpha (LVar var)      = LVar $ if var == v then v' else var
  doAlpha (LApp e1 e2)    = LApp (doAlpha e1) (doAlpha e2)
  doAlpha (LAbs var le)   = LAbs (if var == v then v' else var) (doAlpha le)
-- pouziti: alpha (LAbs "x" (LAbs "y" (LAbs "z" (LApp(LVar "x") (LVar "a"))))) (LVar "x") (LVar "X") == LAbs "X" (LAbs "y" (LAbs "z" (LApp (LVar "X") (LVar "a"))))


-- Substituce
isValidSubst :: Eq a => Lambda a -> Lambda a -> Lambda a -> Bool
isValidSubst e (LVar v) e' = if check then True
				      else False
  where
  check = ((elem v (freeVars e))) && ((intersect (boundingVars e) ((freeVars e') ++ (boundVars e') ++ (boundingVars e'))) == []) -- Nahrazovana promenna musi byt volna a nahrazujici vyraz musi mit jinak pojmenovane promenne
 


-- Substituce
subst :: Eq a => Lambda a -> Lambda a -> Lambda a -> Lambda a
subst e (LVar v) e' = if check then doSubst e
			       else error "Bounding problem!"
  where
  check = (not (elem v (boundVars e))) && ((intersect (boundingVars e) ((freeVars e') ++ (boundVars e') ++ (boundingVars e'))) == []) -- Nahrazovana promenna musi byt volna a nahrazujici vyraz musi mit jinak pojmenovane promenne
  doSubst (LVar v')      = if v' == v then e' else (LVar v')
  doSubst (LApp e1 e2)   = LApp (doSubst e1) (doSubst e2)
  doSubst (LAbs v' le)   = LAbs v' (doSubst le)
-- pouziti: subst (LAbs "x" (LVar "y")) (LVar "y") (LApp (LVar "a") (LVar "b")) == LAbs "x" (LApp (LVar "a") (LVar "b"))

-- Beta redukce
beta :: Eq a => Lambda a -> Lambda a -> Lambda a
beta (LAbs v e) e' = if check then doBeta e 
			      else error "Bounding problem!" 
  where
  check = intersect (filter (/=v) ((freeVars e') ++ (boundingVars e'))) (boundingVars (LAbs v e)) == []
  doBeta (LVar v')       = if v' == v then e' else LVar v'
  doBeta (LApp e1 e2)    = LApp (doBeta e1) (doBeta e2)
  doBeta (LAbs v' le)    = LAbs v' (if v' == v then le else doBeta le)
-- pouziti: beta ( (LAbs "x"(LAbs "z" (LApp(LVar "x") (LVar "z")) ) )  )  (LApp(LVar "a") (LVar "b")) == LAbs "z" (LApp (LApp (LVar "a") (LVar "b")) (LVar "z"))

-- Eta konverze
eta :: Eq a => Lambda a -> Lambda a
eta l@(LAbs v (LApp e (LVar v'))) = if (v == v') && (not $ elem v (freeVars e)) then e else l
-- pouziti: eta (LAbs "z" (LApp (LApp (LVar "a") (LVar "b")) (LVar "z"))) == LApp (LVar "a") (LVar "b")

intersect :: Eq t => [t] -> [t] -> [t]
intersect xs ys = [a | a <- xs, b <- ys, a == b]