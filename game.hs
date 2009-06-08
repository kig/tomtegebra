{-
Introduction

Tomtegebra is a most exciting game revolving around groups, rings and fields.

The game is composed of an adventure part where you find new functions
to prove, and the proof part where you reign over functions by showing
them for what they really are!
-}

module Game where
import Algebra

data AppState = State {
        equation :: Rule,
        cursorLocation :: Int,
        inventory :: ProofInventory,
        inventoryIndex :: Int,
        equationCompleted :: Bool,
        equations :: Axioms,
        levels :: [Level],

        gameOver :: Bool,

        angle :: Double,
        width :: Double,
        height :: Double,
        dir :: Double
    }

type Axioms = [Rule]

data Level = Level (Op, Axioms)

levelO = Level ( "o", [(A `o` B) `eq` ((A `plus` B) `plus` Literal 1)] )

initState :: AppState
initState = nextLevel $ State {
                equation = (A `o` B) `eq` (A `o` B),
                cursorLocation = 0,
                inventory = (field "+" "*"),
                inventoryIndex = -1,
                equationCompleted = False,
                equations = [],
                levels = [levelO],
                gameOver = False,

                angle=0, width=600, height=600, dir=1.0}

resetCursor :: AppState -> AppState
resetCursor sta = sta { cursorLocation = 0, inventoryIndex = -1 }

changeLevel :: Level -> AppState -> AppState
changeLevel (Level (op, axioms)) sta =
    firstEquation (sta { equations = abelianGroup op,
                         inventory = inventory sta ++ axioms })

nextLevel :: AppState -> AppState
nextLevel sta =
    case levels sta of
        [] -> sta { gameOver = True }
        x:xs -> changeLevel x sta { levels = xs }

changeEquation :: Rule -> AppState -> AppState
changeEquation r sta = (resetCursor sta) { equation = r, equationCompleted = False }

firstEquation :: AppState -> AppState
firstEquation sta = 
    case equations sta of
        [] -> nextLevel sta
        x:xs -> changeEquation x sta -- no need to change equations as that's handled by nextEquation

nextEquation :: AppState -> AppState
nextEquation sta =
    case equations sta of
        [x] -> nextLevel $ addToInventory x sta
        [] -> nextLevel sta
        x:y:xs -> changeEquation y $ addToInventory x sta {equations = xs}

moveCursorLeft :: AppState -> AppState
moveCursorLeft = moveCursor (-1)

moveCursorRight :: AppState -> AppState
moveCursorRight = moveCursor 1

moveCursor :: Int -> AppState -> AppState
moveCursor amount sta =
    sta { cursorLocation = (cursorLocation sta + amount) `mod` ruleLength (equation sta) }

scrollInventoryUp :: AppState -> AppState
scrollInventoryUp = scrollInventory (-1)

scrollInventoryDown :: AppState -> AppState
scrollInventoryDown = scrollInventory 1

scrollInventory :: Int -> AppState -> AppState
scrollInventory amount sta = sta { inventoryIndex = inventoryIndex sta + amount }

addToInventory :: Rule -> AppState -> AppState
addToInventory r sta = sta { inventory = inventory sta ++ [r] }

applyCurrentRule :: AppState -> AppState
applyCurrentRule sta =
    sta { equation = equ,
          cursorLocation = cursorLocation sta `mod` ruleLength equ,
          equationCompleted = isTautology equ
        }
    where cloc = cursorLocation sta
          origEq = equation sta
          equE = toExpr origEq
          inv = findMatchingEqualitiesAt cloc equE $ inventory sta
          invIdx = inventoryIndex sta `mod` length inv
          rule = inv !! invIdx
          equ = maybe origEq id (toRule $ applyEqualityAt cloc rule equE)

