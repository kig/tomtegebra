module Algebra (
-- * Algebra gameplay
-- $algebra_gameplay
    isTrue,
    isBinding,
    opf,

-- ** Group axioms
-- $group_axioms
    CheckableRule,
    NamedPredicate,
    checkCheckableRule,
    isTrueC,
    isNeutralC,
    isInverseC,
    associativity,
    leftNeutral,
    rightNeutral,
    leftInverse,
    rightInverse,
    magma,
    semiGroup,
    monoid,
    group,

-- ** Abelian group
-- $abelian_group
    commutativity,
    abelianGroup,

-- ** Ring axioms
-- $ring_axioms
    leftDistributivity,
    rightDistributivity,
    pseudoRing,
    ring,
    commutativeRing,

-- ** Field axioms
-- $field_axioms
    field,

-- * Proof inventory
-- $proof_inventory
    ProofInventory,
    findMatchingEqualitiesAt,
    findMatchingEqualities,
    matchEquality,
    inventoryFor,

-- * Expressions
-- $expressions
    Expr (..),
    Op,
    plus,
    o,

-- ** Rules
-- $rules
    Rule (Rule),
    Pattern,
    Substitution,

    ruleLength,
    reverseRule,
    toExpr,
    toRule,
    eqE,
    eq,
    equalityTransforms,
    eqTrans,
    replaceABCwithXYZ,

-- ** Rewriting
-- $rewriting
    applyRule,
    applyEquality,
    mapExpr,
    exprLength,
    
-- *** Indexing
-- $indexing
    applyRuleAt,
    applyEqualityAt,
    subExprAt,
    outerMapExprWithIndex

) where

import Data.List hiding (group)

{- $algebra_gameplay

The proof gameplay consists of subsequent stages of proving group axioms
for the function by fiddling the axiom equation to show equality or to
bind the wanted variable.

-}

-- | 'isTrue' checks whether both sides of a 'Rule' are equal.
isTrue :: Rule -> Bool
isTrue (Rule (a,b)) = a == b

-- | 'isBinding' checks if a 'Rule' is a binding for 'Expr' (i.e. if one side of the 'Rule' has only 'Expr' on it.)
--   'isBinding' also returns true if both sides of the 'Rule' are equal.
isBinding :: Expr -> Rule -> Bool
isBinding e (Rule (a,b)) = a == b || e == a || e == b

-- | 'opf' is a shortcut for creating 'Expr'-creating functions.
--   E.g. let o = opf "o" in A `o` B
--   instead of let o a b = Expr ("o", a, b) in A `o` B
opf :: Op -> Expr -> Expr -> Expr
opf op a b = Expr (op, a, b)

{- $group_axioms
Group axioms main stage:

    Closure: @a o b exists in G for all a, b in G@

    Magma!

    Associativity: @ a o (b o c) = (a o b) o c @
    
    Semigroup!

    Neutral element:
    @ a o 0 = 0 o a = a @

    Monoid!

    Inverse element:
    @ a o a_inv = a_inv o a = 0 @
    
    Group! Enter Abelian bonus stage!
-}

-- | 'CheckableRule' is a 'Rule' with a named predicate function to check it.
--   E.g. (("bothEqual", isTrue), Rule (foo, bar))
type CheckableRule = ((String, (Rule -> Bool)), Rule)

-- | 'NamedPredicate' is a (name, function) -tuple.
type NamedPredicate = (String, (Rule -> Bool))

-- | 'isTrueC' is a 'NamedPredicate' named "bothEqual" that uses isTrue as its predicate.
isTrueC :: NamedPredicate
isTrueC = ("bothEqual", isTrue)

-- | 'isNeutralC' returns the 'NamedPredicate' for the given 'Expr', named "bindNeutral" and uses isBinding expr as its predicate.
isNeutralC :: Expr -> NamedPredicate
isNeutralC e = ("bindNeutral", isBinding e)

-- | 'isInverseC' returns the 'NamedPredicate' for the given 'Expr', named "bindInverse" and uses isBinding expr as its predicate.
isInverseC :: Expr -> NamedPredicate
isInverseC e = ("bindInverse", isBinding e)

-- | 'checkCheckableRule' applies the predicate of a 'CheckableRule' to its 'Rule' and returns the result.
checkCheckableRule :: CheckableRule -> Bool
checkCheckableRule ((_,p), rule) = p rule

-- | 'associativity' returns the associativity 'CheckableRule' for the given 'Op'.
--   (a o (b o c)) = ((a o b) o c)
associativity :: Op -> CheckableRule
associativity op = (isTrueC, (A `ox` (B `ox` C)) `eq` ((A `ox` B) `ox` C))
                  where ox = opf op

-- | 'rightNeutral' returns the 'CheckableRule' for the right neutral element of the given 'Op'.
--   (a o e) = a
rightNeutral :: Op -> CheckableRule
rightNeutral op = (isNeutralC (Neutral op), (A `ox` Neutral op) `eq` A)
              where ox = opf op

-- | 'leftNeutral' returns the 'CheckableRule' for the left neutral element of the given 'Op'.
--   (e o a) = a
leftNeutral :: Op -> CheckableRule
leftNeutral op = (isNeutralC (Neutral op), (Neutral op `ox` A) `eq` A)
              where ox = opf op

-- | 'rightInverse' returns the 'CheckableRule' for the right inverse function of the given 'Op'.
--   (a o inv(a)) = e
rightInverse :: Op -> CheckableRule
rightInverse op = (isInverseC (Inv (op, A)), (A `ox` Inv (op, A)) `eq` Neutral op)
              where ox = opf op

-- | 'leftInverse' returns the 'CheckableRule' for the left inverse function of the given 'Op'.
--   (inv(a) o a) = e
leftInverse :: Op -> CheckableRule
leftInverse op = (isInverseC (Inv (op, A)), (Inv (op, A) `ox` A) `eq` Neutral op)
              where ox = opf op

{- $abelian_group
Abelian bonus stage:

    Commutativity:
    @a o b = b o a@

    Abelian group! Enter ring bonus stage!
-}

-- | 'commutativity' returns the 'CheckableRule' for the commutativity of the given 'Op'.
--   (a o b) = (b o a)
commutativity :: Op -> CheckableRule
commutativity op = (isTrueC, (A `ox` B) `eq` (B `ox` A))
                  where ox = opf op

-- | 'magma' states that a function is 'G -> G -> G'. It does nothing here, how does one prove it?
magma :: Op -> [CheckableRule]
magma _ = [] -- FIXME?

-- | 'semiGroup' is a 'magma' with 'associativity'.
semiGroup :: Op -> [CheckableRule]
semiGroup op = magma op ++ [associativity op]

-- | 'monoid' is a 'semiGroup' with 'rightNeutral' and 'leftNeutral' (that are equal.)
monoid :: Op -> [CheckableRule]
monoid op = semiGroup op ++ [rightNeutral op, leftNeutral op]

-- | 'group' is a 'monoid' with 'rightInverse' and 'leftInverse' (that are equal.)
group :: Op -> [CheckableRule]
group op = monoid op ++ [rightInverse op, leftInverse op]

-- | 'abelianGroup' is a 'group' with 'commutativity'.
abelianGroup :: Op -> [CheckableRule]
abelianGroup op = group op ++ [commutativity op]


{- $ring_axioms
Ring bonus stage:

    Show bonus function to be a semigroup!

    Distributivity of x over o:
@    a x (b o c) = (a x b) o (a x c)@

@    (a o b) x c = (a x c) o (b x c)@
    
    Pseudo-ring!

    Neutral element for x:
@    a x 1 = 1 x a = a@

    Ring!

    Commutativity for x:
@    a x b = b x a@

    Commutative ring! Enter field bonus stage!

-}

-- | 'leftDistributivity' returns the 'CheckableRule' for the left distributivity of
--   opX over opO, i.e. a x (b o c) = (a x b) o (a x c).
leftDistributivity :: Op -> Op -> CheckableRule
leftDistributivity opO opX = (isTrueC, (A `x` (B `ox` C)) `eq` ((A `x` B) `ox` (A `x` C)))
                             where ox = opf opO
                                   x = opf opX

-- | 'rightDistributivity' returns the 'CheckableRule' for the right distributivity of 
--   opX over opO, i.e. (a o b) x c = (a x c) o (b x c).
rightDistributivity :: Op -> Op -> CheckableRule
rightDistributivity opO opX = (isTrueC, ((A `ox` B) `x` C) `eq` ((A `x` C) `ox` (B `x` C)))
                               where ox = opf opO
                                     x = opf opX


{- $field_axioms
Field bonus stage:

    Inverse element for x in G \ {0}:
@    a x a_inv = a_inv x a = 1@

    Field! Superior! Shower of jewels!
-}


-- | A 'pseudoRing' is an 'abelianGroup' o with a 'semiGroup' x where x is distributive over o.
pseudoRing :: Op -> Op -> [CheckableRule]
pseudoRing ox x = abelianGroup ox ++ semiGroup x ++
                 [leftDistributivity ox x, rightDistributivity ox x]

-- | A 'ring' is a 'pseudoRing' o x with the neutral element for x.
ring :: Op -> Op -> [CheckableRule]
ring ox x = pseudoRing ox x ++ [rightNeutral x, leftNeutral x]

-- | A 'commutativeRing' is a 'ring' o x with commutativity for x.
commutativeRing :: Op -> Op -> [CheckableRule]
commutativeRing ox x = ring ox x ++ [commutativity x]

-- | A 'field' is a 'commutativeRing' o x with the inverse function for x 
--   (in G \\ {neutral(o)}).
field :: Op -> Op -> [CheckableRule]
field ox x = commutativeRing ox x ++ [rightInverse x, leftInverse x]


{- $proof_inventory
Whenever you show something, the equality is added to your proof inventory.
You can use items in your proof inventory to do substitutions and other manipulations.
You begin the game armed with +. Build it up from there.
-}

type ProofInventory = [Rule]

-- | Finds the matching rules in an inventory for the subexpression at the given
--   index of 'Expr'.
findMatchingEqualitiesAt :: Int -> Expr -> ProofInventory -> ProofInventory
findMatchingEqualitiesAt idx expr inventory =
    maybe [] (\e -> findMatchingEqualities e inventory) (subExprAt idx expr)

-- | Finds the matching rules in an inventory for the given expression.
findMatchingEqualities :: Expr -> ProofInventory -> ProofInventory
findMatchingEqualities expr inventory = filter (matchEquality expr) inventory

-- | Checks if the 'Rule' pattern matches the 'Expr'. I.e. if there's a valid
--   binding for either side of the 'Rule' in 'Expr'
matchEquality :: Expr -> Rule -> Bool
matchEquality e (Rule (a,b)) = matchPattern a e || matchPattern b e

{- $expressions

Functions are composed of explicitly parenthesized binary operators and variables.
The names of the variables are always A and B.

E.g. the function

>    f a b = a + b + 1

would be represented as

@    (+ (+ a b) 1)@

@    Expr (Plus, (Expr (Plus, A, B), 1)@

-}

type Op = String
data Expr = Expr (Op, Expr, Expr)
          | A | B | C | X | Y | Z
          | Inv (Op, Expr)
          | Neutral Op
          | Literal Int

instance Show Expr where
    show (Expr (ox,l,r)) = "("++show l++" "++ox++" "++show r++")"
    show A = "a"
    show B = "b"
    show C = "c"
    show X = "x"
    show Y = "y"
    show Z = "z"
    show (Inv (ox,e)) = "inv("++ox++","++show e++")"
    show (Neutral ox) = "e("++ox++")"
    show (Literal i) = show i

instance Eq Expr where
    Expr a == Expr b = a == b
    A == A = True
    B == B = True
    C == C = True
    X == X = True
    Y == Y = True
    Z == Z = True
    Inv a == Inv b = a == b
    Neutral aop == Neutral bop = aop == bop
    Literal a == Literal b = a == b
    _ == _ = False

{- $rules

The core of the gameplay is based on rewriting equations.
To rewrite an equation, you apply a rule to an expression.

E.g. the rule

>    a o b = a + b + 1

would be represented as

@    rule = Rule ((Expr (\"o\", A, B)), (Expr (\"+\", Expr (\"+\", A, B), Literal 1)))@
-}

type Pattern = Expr
type Substitution = Expr

-- | A Rule is a expression transformer; it binds the 'Pattern' to an 'Expr' and
--   replaces the 'Expr' with the 'Substitution' evaluated in the aforementioned
--   binding.
--
--   E.g. If you apply the rule @A = 0 + A@ to @((A + B) * C)@, you would get
--        @0 + ((A + B) * C)@.
data Rule = Rule (Pattern, Substitution)
instance Show Rule where
    show (Rule (a, b)) = show a ++ " = " ++ show b

instance Eq Rule where
    Rule a == Rule b = a == b

-- | The length of a 'Rule' is the sum of the lengths of its two expressions, plus one.
ruleLength :: Rule -> Int
ruleLength (Rule (a, b)) = exprLength a + 1 + exprLength b

-- | Swaps the left and right sides of the 'Rule'.
reverseRule :: Rule -> Rule
reverseRule (Rule (a, b)) = Rule (b, a)

-- | The 'Expr' corresponding to @'Rule' (a,b)@ is @'Expr' (\"=\",a,b)@
toExpr :: Rule -> Expr
toExpr (Rule (a,b)) = Expr ("=", a, b)

-- | Tries to convert the given 'Expr' to a 'Rule'. Only succeeds if the
--   expression is of form @'Expr' (\"=\", a, b)@
toRule :: Expr -> Maybe Rule
toRule (Expr ("=", a, b)) = Just (Rule (a, b))
toRule _ = Nothing

-- | Convenience function for creating an equality expression
--   @a \`eqE\` b = Expr (\"=\", a, b)@
eqE :: Expr -> Expr -> Expr
eqE a b = Expr ("=",a,b)

-- | An equality transform says that the boolean binary operator \"=\" has
--   the following property: @(A = B) = (A op C = B op C)@.
eqTrans :: Op -> Expr -> Rule
--   Reversed here so that term-removing side binds first, otherwise we couldn't
--   remove added terms.
eqTrans op c = ((X `ox` c) `eqE` (Y `ox` c)) `eq` (X `eqE` Y)
               where ox = opf op

-- | Builds the list of equality transforms for 'Expr' by creating an equality
--   transform for each op, variable -pair in 'Expr', including variable inverses.
--
--   An equality transform is @(A = B) = (A op C = B op C)@.
equalityTransforms :: Expr -> [Rule]
equalityTransforms (Expr ("=", l, r)) =
    nub (equalityTransforms l ++ equalityTransforms r)
equalityTransforms e@(Expr (ox, _, _)) = map (eqTrans ox) $ expandVars e (getUniqueVariables e)
equalityTransforms _ = []

expandVars :: Expr -> [Expr] -> [Expr]
expandVars expr vars = concatMap (expandVar expr) vars

expandVar :: Expr -> Expr -> [Expr]
expandVar expr =
    let ops = getUniqueOps expr in
    (\v -> concatMap (\op -> case v of
                                Inv (_,v') -> [v']
                                _ -> [v, Inv (op, v)]) ops)

getUniqueOps :: Expr -> [Op]
getUniqueOps e = nub $ getOps e

getOps :: Expr -> [Op]
getOps (Expr (ox, l, r)) = ox : (getOps l ++ getOps r)
getOps _ = []

getUniqueVariables :: Expr -> [Expr]
getUniqueVariables e = nub $ getVariables e

getVariables :: Expr -> [Expr]
getVariables (Expr (_, l, r)) = getVariables l ++ getVariables r
getVariables x = [x]

-- | Returns the proof inventory for the subexpression of 'CheckableRule' at
--   the given index.
--
--   On a non-equality subexpression, filters the given 'ProofInventory' for
--   matching rules. On the equality subexpression (=), returns a list of
--   'equalityTransforms' for the 'CheckableRule'.
inventoryFor :: Int -> CheckableRule -> ProofInventory -> ProofInventory
inventoryFor idx (_, rule) inventory =
    filter (includeSameOp rule) unfiltered
    where subExpr = subExprAt idx (toExpr rule)
          unfiltered = maybe [] subExprInventory subExpr
          subExprInventory e@(Expr ("=", _, _)) = equalityTransforms e
          subExprInventory e = findMatchingEqualities e inventory
          includeSameOp r1 r2 = findMatchFromSorted (uniqOps r1) (uniqOps r2)
          uniqOps = sort . without "=" . getUniqueOps . toExpr
          without e = filter (/= e)

findMatchFromSorted :: Ord a => [a] -> [a] -> Bool
findMatchFromSorted (x:xs) (y:ys)
    | x == y = True
    | x < y = findMatchFromSorted xs (y:ys)
    | x > y = findMatchFromSorted (x:xs) ys
findMatchFromSorted _ _ = False

-- | Replaces the A, B and C leafs in the Rule's expressions by X, Y, and Z, respectively.
replaceABCwithXYZ :: Rule -> Rule
replaceABCwithXYZ (Rule (l,r)) = Rule (mapExpr exprABCtoXYZ l, mapExpr exprABCtoXYZ r)

exprABCtoXYZ :: Expr -> Expr
exprABCtoXYZ A = X
exprABCtoXYZ B = Y
exprABCtoXYZ C = Z
exprABCtoXYZ (Inv (ox,e)) = Inv (ox, exprABCtoXYZ e)
exprABCtoXYZ e = e

{- $rewriting

The rewriting is done by attempting to bind the pattern to a given expression,
and on success, evaluating the substitution in that binding.

If the pattern doesn't match the expression, rewriting will return the original
expression.

E.g. applying the above rule a o b = a + b + 1 to ((a + b) o b):

@    applyRule rule (Expr (\"o\", Expr (\"+\", A, B), B))@

would result in

@    Expr (\"+\", Expr (\"+\", Expr (\"+\", A, B), B), Literal 1)@

-}

-- | Applies the transformation 'Rule' to the expression 'Expr'.
--   If the left side of 'Rule' doesn't bind, returns the expression unchanged.
applyRule :: Rule -> Expr -> Expr
applyRule (Rule (pat, sub)) expression =
    maybe expression id (evalExpr binding sub)
    where binding = bindPattern pat expression

-- | Applies the two-sided transformation 'Rule' to 'Expr'.
--   If neither side of 'Rule' doesn't bind (left side tried first), returns the
--   expression unchanged.
applyEquality :: Rule -> Expr -> Expr
applyEquality r@(Rule (pat, _)) expr =
    if matchPattern pat expr
        then applyRule r expr
        else applyRule (reverseRule r) expr

data Binding = Binding (Maybe Expr, Maybe Expr, Maybe Expr, Maybe Expr, Maybe Expr, Maybe Expr) 
             | Invalid

emptyBinding :: Binding
emptyBinding = Binding (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

invalidBinding :: Binding -> Bool
invalidBinding Invalid = True
invalidBinding _ = False

evalExpr :: Binding -> Expr -> Maybe Expr
evalExpr binding _ | invalidBinding binding = Nothing
evalExpr binding e = Just (mapExpr (applyBinding binding) e)

-- | Structure-preserving map over Expr.
mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f (Expr (ox, l, r)) = Expr (ox, mapExpr f l, mapExpr f r)
mapExpr f x = f x

-- | Length of the 'Expr'. Variables, literals, inverses and neutrals count as one.
--   'Expr' nodes count as 1 plus the sum of the left and right sides.
exprLength :: Expr -> Int
exprLength (Expr (_, l, r)) = 1 + exprLength l + exprLength r
exprLength _ = 1

applyBinding :: Binding -> Expr -> Expr
applyBinding (Binding (Just a,_,_,_,_,_)) A = a
applyBinding (Binding (_,Just b,_,_,_,_)) B = b
applyBinding (Binding (_,_,Just c,_,_,_)) C = c
applyBinding (Binding (_,_,_,Just x,_,_)) X = x
applyBinding (Binding (_,_,_,_,Just y,_)) Y = y
applyBinding (Binding (_,_,_,_,_,Just z)) Z = z
applyBinding _ e = e

bindPattern :: Pattern -> Expr -> Binding
bindPattern pat expr =
    updateBinding emptyBinding pat expr

updateBinding :: Binding -> Pattern -> Expr -> Binding
updateBinding Invalid _ _ = Invalid

updateBinding (Binding (Nothing, b, c,x,y,z)) A a = Binding (Just a, b, c,x,y,z)
updateBinding (Binding (Just a, b, c,x,y,z)) A ae | a == ae = Binding (Just a, b, c,x,y,z)
updateBinding _ A _ = Invalid

updateBinding (Binding (a, Nothing, c,x,y,z)) B b = Binding (a, Just b, c,x,y,z)
updateBinding (Binding (a, Just b, c,x,y,z)) B be | b == be = Binding (a, Just b, c,x,y,z)
updateBinding _ B _ = Invalid

updateBinding (Binding (a, b, Nothing,x,y,z)) C c = Binding (a, b, Just c,x,y,z)
updateBinding (Binding (a, b, Just c,x,y,z)) C ce | c == ce = Binding (a, b, Just c,x,y,z)
updateBinding _ C _ = Invalid

updateBinding (Binding (a, b, c,Nothing,y,z)) X x = Binding (a, b, c,Just x,y,z)
updateBinding (Binding (a, b, c,Just x,y,z)) X xe | x == xe = Binding (a, b, c, Just x,y,z)
updateBinding _ X _ = Invalid

updateBinding (Binding (a, b, c,x,Nothing,z)) Y y = Binding (a, b, c,x,Just y,z)
updateBinding (Binding (a, b, c,x,Just y,z)) Y ye | y == ye = Binding (a, b, c, x,Just y,z)
updateBinding _ Y _ = Invalid

updateBinding (Binding (a, b, c,x,y,Nothing)) Z z = Binding (a, b, c,x,y,Just z)
updateBinding (Binding (a, b, c,x,y,Just z)) Z ze | z == ze = Binding (a, b, c, x,y,Just z)
updateBinding _ Z _ = Invalid


updateBinding b (Literal x) (Literal y) | x == y = b
updateBinding _ (Literal _) _ = Invalid

updateBinding b (Inv (op,e)) (Inv (ope,ee)) | op == ope = updateBinding b e ee
updateBinding _ (Inv _) _ = Invalid

updateBinding b (Expr (op, pl, pr)) (Expr (ope, el, er)) | op == ope =
    updateBinding (updateBinding b pl el) pr er
updateBinding _ (Expr _) _ = Invalid

updateBinding b (Neutral op) (Neutral ope) | op == ope = b
updateBinding _ (Neutral _) _ = Invalid

matchPattern :: Pattern -> Expr -> Bool
matchPattern pat expr = not $ invalidBinding (bindPattern pat expr)

-- |Convenience function for creating expressions, @A \`plus\` B = Expr (\"+\", a, b)@.
plus :: Expr -> Expr -> Expr
a `plus` b = Expr ("+", a, b)

-- |Convenience function for creating expressions, @A \`o\` B = Expr (\"o\", a, b)@.
o :: Expr -> Expr -> Expr
a `o` b = Expr ("o", a, b)

-- |Convenience function for 'Rule' creation, @A \`eq\` B = Rule (a,b)@.
eq :: Expr -> Expr -> Rule
a `eq` b = Rule (a, b)

{- $indexing

To apply a rewrite rule to a particular place in an equation, you need to be
able to index the symbols in the equation from left to right.

E.g.

>    applyRuleAt 1 ((A `plus` B) `eq` (A `o` B)) ((A `plus` Literal 1) `plus` B)

would result in @((a o 1) + b)@ instead of @((a + 1) o b)@
-}

-- | Applies the transformation 'Rule' at index 'Int' in 'Expr'.
--   If the left side of 'Rule' doesn't match 'Expr', returns 'Expr' unchanged.
applyRuleAt :: Int -> Rule -> Expr -> Expr
applyRuleAt idx rule expr =
    outerMapExprWithIndex (\e i -> if i == idx then applyRule rule e else e) expr

-- | Applies the two-sided transformation 'Rule' at index 'Int' in 'Expr'.
--   If neither side of 'Rule' matches 'Expr', returns 'Expr' unchanged.
--   Left side of the 'Rule' is tried first.
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

-- | Returns the subexpression at index 'Int' of 'Expr'.
--   If 'Int' is out of 'Expr' bounds, returns Nothing.
subExprAt :: Int -> Expr -> Maybe Expr
subExprAt idx expr = fst $ subExprAt' expr idx 0

subExprAt' :: Expr -> Int -> Int -> (Maybe Expr, Int)
subExprAt' _ i c | c > i = (Nothing, 0)
subExprAt' e@(Expr (_,l,r)) idx count =
    let (mex, c) = subExprAt' l idx count in
    case mex of
        Just _ -> (mex,c)
        Nothing ->
            if c == idx
                then (Just e, c)
                else subExprAt' r idx (c+1)
subExprAt' e i c | i == c = (Just e, c)
subExprAt' _ _ c = (Nothing, c+1)

-- | Structure-altering left-to-right map over an 'Expr'.
--
--   If an 'Expr' node is changed before going down its right side,
--   the right side of the new node is traversed instead.
outerMapExprWithIndex :: (Expr -> Int -> Expr) -> Expr -> Expr
outerMapExprWithIndex f expr = snd $ outerMapExprWithIndex' f expr 0

outerMapExprWithIndex' :: (Expr -> Int -> Expr) -> Expr -> Int -> (Int, Expr)
outerMapExprWithIndex' f (Expr (ox,a,b)) c =
    let (c', left) = outerMapExprWithIndex' f a c in
    let (c'', expr) = (c'+1, f (Expr (ox, left, b)) c') in
    case expr of
        Expr (o',left',b') -> let (c''', right) = outerMapExprWithIndex' f b' c'' in
                           (c''', Expr (o', left', right))
        x -> outerMapExprWithIndex' f x c''

outerMapExprWithIndex' f x c = (c+1, f x c)
