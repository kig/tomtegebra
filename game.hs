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
        equation :: CheckableRule,
        cursorLocation :: Int,
        inventory :: ProofInventory,
        inventoryIndex :: Int,
        equationCompleted :: Bool,
        equations :: [CheckableRule],
        levels :: [Level],

        gameOver :: Bool,

        angle :: Double,
        width :: Double,
        height :: Double,
        dir :: Double
    }

type Rules = [Rule]

data Level = Level (Op, Rules)

levelO = Level ( "o", [(A `o` B) `eq` ((A `plus` B) `plus` Literal 1)] )

initState :: AppState
initState = nextLevel $ State {
                equation = (isTrue, (A `o` B) `eq` (A `o` B)),
                cursorLocation = 0,
                inventory = map snd (field "+" "*"),
                inventoryIndex = -1,
                equationCompleted = False,
                equations = [],
                levels = [levelO],
                gameOver = False,

                angle=0, width=600, height=600, dir=1.0}

resetCursor :: AppState -> AppState
resetCursor sta = sta { cursorLocation = 0, inventoryIndex = -1 }

changeLevel :: Level -> AppState -> AppState
changeLevel (Level (op, rules)) sta =
    firstEquation (sta { equations = abelianGroup op,
                         inventory = inventory sta ++ rules })

nextLevel :: AppState -> AppState
nextLevel sta =
    case levels sta of
        [] -> sta { gameOver = True }
        x:xs -> changeLevel x sta { levels = xs }

changeEquation :: CheckableRule -> AppState -> AppState
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
    sta { cursorLocation = (cursorLocation sta + amount) `mod` ruleLength (snd $ equation sta) }

scrollInventoryUp :: AppState -> AppState
scrollInventoryUp = scrollInventory (-1)

scrollInventoryDown :: AppState -> AppState
scrollInventoryDown = scrollInventory 1

scrollInventory :: Int -> AppState -> AppState
scrollInventory amount sta = sta { inventoryIndex = inventoryIndex sta + amount }

addToInventory :: CheckableRule -> AppState -> AppState
addToInventory r sta = sta { inventory = inventory sta ++ [snd r] }

applyCurrentRule :: AppState -> AppState
applyCurrentRule sta =
    sta { equation = equ,
          cursorLocation = cursorLocation sta `mod` ruleLength (snd equ),
          equationCompleted = checkCheckableRule equ
        }
    where cloc = cursorLocation sta
          origEq = equation sta
          equE = toExpr (snd origEq)
          inv = inventoryFor cloc origEq $ inventory sta
          invIdx = inventoryIndex sta `mod` length inv
          rule = inv !! invIdx
          equ = maybe origEq (\s-> (fst origEq,s)) (toRule $ applyEqualityAt cloc rule equE)

