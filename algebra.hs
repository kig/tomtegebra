{-
{Algebra gameplay}

The proof gameplay consists of subsequent stages of proving group axioms
for the function by fiddling the axiom equation to show equality.
-}
module Algebra where

isTautology :: Rule -> Bool
isTautology (Rule (a,b)) = a == b

isBinding :: Expr -> Rule -> Bool
isBinding e (Rule (a,b)) = e == a || e == b

opf :: Op -> Expr -> Expr -> Expr
opf op a b = Expr (op, a, b)

{-
Group axioms main stage:

    Stability: a o b exists in G for all a, b in G
    Magma!

    Associativity: a o (b o c) = (a o b) o c
    Semigroup!

    Neutral element: a o 0 = 0 o a = a
    Monoid!

    Inverse element: a o a_inv = a_inv o a = 0
    Group! Enter Abelian bonus stage!
-}

associativity :: Op -> Rule
associativity op = (A `o` (B `o` C)) `eq` ((A `o` B) `o` C)
                  where o = opf op

rightNeutral :: Op -> Rule
rightNeutral op = (A `o` Neutral op) `eq` A
              where o = opf op

leftNeutral :: Op -> Rule
leftNeutral op = (Neutral op `o` A) `eq` A
              where o = opf op

rightInverse :: Op -> Rule
rightInverse op = (A `o` Inv (op, A)) `eq` Neutral op
              where o = opf op

leftInverse :: Op -> Rule
leftInverse op = (Inv (op, A) `o` A) `eq` Neutral op
              where o = opf op

{-
Abelian bonus stage:

    Commutativity: a o b = b o a
    Abelian group! Enter ring bonus stage!
-}

commutativity :: Op -> Rule
commutativity op = (A `o` B) `eq` (B `o` A)
                  where o = opf op

magma op = [] -- FIXME: figure out some way to prove that op :: G -> G -> G
semiGroup op = magma op ++ [associativity op]
monoid op = semiGroup op ++ [rightNeutral op, leftNeutral op]
group op = monoid op ++ [rightInverse op, leftInverse op]
abelianGroup op = group op ++ [commutativity op]

{-
Ring bonus stage:

    Show bonus function to be a semigroup!

    Distributivity of x over o:
    a x (b o c) = (a x b) o (a x c)
    (a o b) x c = (a x c) o (b x c)
    Pseudo-ring!
-}

leftDistributivity :: Op -> Op -> Rule
leftDistributivity opO opX = (A `x` (B `o` C)) `eq` ((A `x` B) `o` (B `x` C))
                             where o = opf opO
                                   x = opf opX

rightDistributivity :: Op -> Op -> Rule
rightDistributivity opO opX = (A `x` (B `o` C)) `eq` ((A `x` B) `o` (B `x` C))
                               where o = opf opO
                                     x = opf opX
{-
    Neutral element for x: a x 1 = 1 x a = a
    Ring!

    Commutativity for x: a x b = b x a
    Commutative ring! Enter field bonus stage!

Field bonus stage:

    Inverse element for x in G \ {0}: a x a_inv = a_inv x a = 1
    Field! Superior! Shower of jewels!
-}

pseudoRing o x = abelianGroup o ++ semiGroup x ++
                 [leftDistributivity o x, rightDistributivity o x]
ring o x = pseudoRing o x ++ [rightNeutral x, leftNeutral x]
commutativeRing o x = ring o x ++ [commutativity x]
field o x = commutativeRing o x ++ [rightInverse x, leftInverse x]
{-
Whenever you show something, the equality is added to your proof inventory.
You can use items in your proof inventory to do substitutions and other manipulations.
You begin the game armed (in the tutorial) with +. Build it up from there.
-}

type ProofInventory = [Rule]

findMatchingEqualitiesAt :: Int -> Expr -> ProofInventory -> ProofInventory
findMatchingEqualitiesAt idx expr inventory =
    maybe [] (\e -> filter (matchEquality e) inventory) (subExprAt idx expr)

matchEquality :: Expr -> Rule -> Bool
matchEquality e (Rule (a,b)) = matchPattern a e || matchPattern b e

{-
{Functions}

Functions are composed of explicitly parenthesized binary operators and variables.
The names of the variables are always A and B.

E.g. the function
    f a b = a + b + 1
would be represented as
    (+ (+ a b) 1)
    Expr (Plus, (Expr (Plus, A, B), 1)

-}

type Op = String
data Expr = Expr (Op, Expr, Expr) | A | B | C | Inv (Op, Expr) | Neutral Op | Literal Int

instance Show Expr where
    show (Expr (o,l,r)) = "("++show l++" "++o++" "++show r++")"
    show A = "a"
    show B = "b"
    show C = "c"
    show (Inv (o,e)) = "inv("++o++","++show e++")"
    show (Neutral o) = "e("++o++")"
    show (Literal i) = show i

instance Eq Expr where
    Expr a == Expr b = a == b
    A == A = True
    B == B = True
    C == C = True
    Inv a == Inv b = a == b
    Neutral aop == Neutral bop = aop == bop
    Literal a == Literal b = a == b
    _ == _ = False

{-
{Rules}

The core of the gameplay is based on rewriting equations.
To rewrite an equation, you apply a rule to an expression.

E.g. the rule
    a o b = a + b + 1
would be represented as
    rule = Rule ((Expr ("o", A, B)), (Expr ("+", Expr ("+", A, B), Literal 1)))
-}

type Pattern = Expr
type Substitution = Expr

data Rule = Rule (Pattern, Substitution)
instance Show Rule where
    show (Rule (a, b)) = show a ++ " = " ++ show b

instance Eq Rule where
    Rule a == Rule b = a == b

ruleLength :: Rule -> Int
ruleLength (Rule (a, b)) = exprLength a + 1 + exprLength b

reverseRule :: Rule -> Rule
reverseRule (Rule (a, b)) = Rule (b, a)

toExpr :: Rule -> Expr
toExpr (Rule (a,b)) = Expr ("=", a, b)

toRule :: Expr -> Maybe Rule
toRule (Expr ("=", a, b)) = Just (Rule (a, b))
toRule _ = Nothing
{-

The rewriting is done by attempting to bind the pattern to a given expression,
and on success, evaluating the substitution in that binding.

If the pattern doesn't match the expression, rewriting will return the original
expression.

E.g. applying the above rule "a o b = a + b + 1" to "((a + b) o b)"
    applyRule rule (Expr ("o", Expr ("+", A, B), B))
would result in
    Expr ("+", Expr ("+", Expr ("+", A, B), B), Literal 1)

-}
applyRule :: Rule -> Expr -> Expr
applyRule (Rule (pat, sub)) expression =
    maybe expression id (evalExpr binding sub)
    where binding = bindPattern pat expression

applyEquality :: Rule -> Expr -> Expr
applyEquality r@(Rule (pat, sub)) expr =
    if matchPattern pat expr
        then applyRule r expr
        else applyRule (reverseRule r) expr

data Binding = Binding (Maybe Expr, Maybe Expr, Maybe Expr) | Invalid

emptyBinding :: Binding
emptyBinding = Binding (Nothing, Nothing, Nothing)

invalidBinding :: Binding -> Bool
invalidBinding Invalid = True
invalidBinding _ = False

evalExpr :: Binding -> Expr -> Maybe Expr
evalExpr binding e | invalidBinding binding = Nothing
evalExpr binding e = Just (mapExpr (applyBinding binding) e)

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f (Expr (o, l, r)) = Expr (o, mapExpr f l, mapExpr f r)
mapExpr f (Inv (o, e)) = Inv (o, mapExpr f e)
mapExpr f x = f x

exprLength :: Expr -> Int
exprLength (Expr (o, l, r)) = 1 + exprLength l + exprLength r
exprLength (Inv (o, e)) = 1 + exprLength e
exprLength x = 1

applyBinding :: Binding -> Expr -> Expr
applyBinding (Binding (Just a,_,_)) A = a
applyBinding (Binding (_,Just b,_)) B = b
applyBinding (Binding (_,_,Just c)) C = c
applyBinding _ e = e

bindPattern :: Pattern -> Expr -> Binding
bindPattern pat expr =
    updateBinding emptyBinding pat expr

updateBinding :: Binding -> Pattern -> Expr -> Binding
updateBinding Invalid _ _ = Invalid

updateBinding (Binding (Nothing, b, c)) A a = Binding (Just a, b, c)
updateBinding (Binding (Just a, b, c)) A ae | a == ae = Binding (Just a, b, c)
updateBinding _ A _ = Invalid

updateBinding (Binding (a, Nothing, c)) B b = Binding (a, Just b, c)
updateBinding (Binding (a, Just b, c)) B be | b == be = Binding (a, Just b, c)
updateBinding _ B _ = Invalid

updateBinding (Binding (a, b, Nothing)) C c = Binding (a, b, Just c)
updateBinding (Binding (a, b, Just c)) C ce | c == ce = Binding (a, b, Just c)
updateBinding _ C _ = Invalid

updateBinding b (Literal x) (Literal y) | x == y = b
updateBinding _ (Literal _) _ = Invalid

updateBinding b (Inv (op, p)) (Inv (ope, e)) | op == ope = updateBinding b p e
updateBinding _ (Inv _) _ = Invalid

updateBinding b (Expr (op, pl, pr)) (Expr (ope, el, er)) | op == ope =
    updateBinding (updateBinding b pl el) pr er
updateBinding _ (Expr _) _ = Invalid

updateBinding b (Neutral op) (Neutral ope) | op == ope = b
updateBinding b (Neutral _) _ = Invalid

matchPattern :: Pattern -> Expr -> Bool
matchPattern pat expr = not $ invalidBinding (bindPattern pat expr)

a `plus` b = Expr ("+", a, b)
a `o` b = Expr ("o", a, b)
a `eq` b = Rule (a, b)

{-
{Indexing}

To apply a rewrite rule to a particular place in an equation, you need to be
able to index the symbols in the equation from left to right.

E.g.
    applyRuleAt 1 ((A `plus` B) `eq` (A `o` B)) ((A `plus` Literal 1) `plus` B)
would result in
    ((a o 1) + b) instead of ((a + 1) o b)
-}

applyRuleAt :: Int -> Rule -> Expr -> Expr
applyRuleAt idx rule expr =
    outerMapExprWithIndex (\e i -> if i == idx then applyRule rule e else e) expr

applyEqualityAt :: Int -> Rule -> Expr -> Expr
applyEqualityAt idx rule expr =
    outerMapExprWithIndex (\e i -> if i == idx then applyEquality rule e else e) expr
{-

Doing the indexed rule application with an outer map works, but isn't very
pretty or effective. The outer map can also be put into an infinite loop
(not that applyRuleAt does that.)

An alternative way would be to have a list presentation for the expressions and
work with them. Or mapAtIndex.

-}

subExprAt :: Int -> Expr -> Maybe Expr
subExprAt idx expr = fst $ subExprAt' expr idx 0

subExprAt' :: Expr -> Int -> Int -> (Maybe Expr, Int)
subExprAt' _ i c | c > i = (Nothing, 0)
subExprAt' e@(Expr (o,l,r)) idx count =
    let (mex, c) = subExprAt' l idx count in
    case mex of
        Just ex -> (mex,c)
        Nothing ->
            if c == idx
                then (Just e, c)
                else subExprAt' r idx (c+1)
subExprAt' e i c | i == c = (Just e, c)
subExprAt' (Inv (o,e)) idx count = subExprAt' e idx (count+1)
subExprAt' e i c = (Nothing, c+1)

outerMapExprWithIndex :: (Expr -> Int -> Expr) -> Expr -> Expr
outerMapExprWithIndex f expr = snd $ outerMapExprWithIndex' f expr 0

outerMapExprWithIndex' :: (Expr -> Int -> Expr) -> Expr -> Int -> (Int, Expr)
outerMapExprWithIndex' f (Expr (o,a,b)) c =
    let (c', left) = outerMapExprWithIndex' f a c in
    let (c'', expr) = (c'+1, f (Expr (o, left, b)) c') in
    case expr of
        Expr (o',left',b') -> let (c''', right) = outerMapExprWithIndex' f b' c'' in
                           (c''', Expr (o', left', right))
        x -> outerMapExprWithIndex' f x c''

outerMapExprWithIndex' f (Inv (o,e)) c =
    let (c', e') = (c+1, f e' c) in
    case e' of
        Inv (o',e'') -> let (c'', e''') = outerMapExprWithIndex' f e'' c' in
                        (c'', Inv (o', e'''))
        x -> outerMapExprWithIndex' f x c'

outerMapExprWithIndex' f x c = (c+1, f x c)
