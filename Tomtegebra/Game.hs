{-
Introduction

Tomtegebra is a most exciting game revolving around groups, rings and fields.

The game is composed of an adventure part where you find new functions
to prove, and the proof part where you reign over functions by showing
them for what they really are!
-}

module Game where
import Graphics.Rendering.OpenGL (GLfloat)
import Algebra
import Models
import Data.IORef
import Graphics.UI.Gtk.Pango.Layout
import Paths_tomtegebra

data AppState = State {
        equation :: CheckableRule,
        cursorLocation :: Int,
        inventory :: ProofInventory,
        inventoryIndex :: Int,
        equationCompleted :: Bool,
        equations :: [CheckableRule],
        levels :: [Level],

        gameOver :: Bool,

        texts :: [(String,(Int, Int, Model))],
        models :: [(String, Model)],
        angle :: GLfloat,
        width :: GLfloat,
        height :: GLfloat,
        dir :: GLfloat
    }

type Rules = [Rule]

data Level = Level (Op, Rules)


levelCat = Level ( "o", [(A `o` B) `eq` ((A `plus` B) `plus` Literal 1)] )

levelFrog = Level ( "f", [(A `f` B) `eq` ((A `o` Literal 1) `o` B)])
            where f = opf "f"

levelBunny = Level ( "x", [(A `x` B) `eq` ((Literal 1 `f` B) `f` A)])
            where x = opf "x"
                  f = opf "f"


initGame :: [(String,(Int, Int, Model))] -> [(String,Model)] -> AppState
initGame texts models = 
    defaultValues $ State {
        equation = (isTrueC, (A `o` B) `eq` (A `o` B)),
        cursorLocation = 0,
        inventory = [],
        inventoryIndex = -1,
        equationCompleted = True,
        equations = [],
        levels = [],
        gameOver = True,
        texts = texts,
        models = models,
        
        angle=0, width=600, height=600, dir=1.0}

resetGame :: AppState -> AppState
resetGame st = (defaultValues st) { equationCompleted = False, gameOver = False }

defaultValues :: AppState -> AppState
defaultValues st =
    st {levels = [levelCat, levelFrog, levelBunny]
        , cursorLocation = 0
        , inventory = map (\(_,r) -> replaceABCwithXYZ r) (abelianGroup "+")
        , inventoryIndex = -1
        , equations = []}


resetCursor :: AppState -> AppState
resetCursor sta = sta { cursorLocation = 0, inventoryIndex = -1 }

changeLevel :: Level -> AppState -> AppState
changeLevel (Level (op, rules)) sta =
    firstEquation (sta { equations = abelianGroup op,
                         inventory = inventory sta ++ map replaceABCwithXYZ rules })

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
        x:y:xs -> changeEquation y $ addToInventory x sta {equations = y:xs}

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
addToInventory r sta = sta { inventory = inventory sta ++ [replaceABCwithXYZ $ snd r] }

applyCurrentRule :: AppState -> AppState
applyCurrentRule sta =
    applyCurrentRule' sta inv cloc origEq
    where inv = inventoryFor cloc origEq $ inventory sta
          cloc = cursorLocation sta
          origEq = equation sta

applyCurrentRule' sta [] cloc origEq = sta
applyCurrentRule' sta inv cloc origEq =
    sta { equation = equ,
          cursorLocation = cursorLocation sta `mod` ruleLength (snd equ),
          equationCompleted = checkCheckableRule equ }
    where equE = toExpr (snd origEq)
          invIdx = inventoryIndex sta `mod` length inv
          rule = inv !! invIdx
          equ = maybe origEq (\s-> (fst origEq,s)) (toRule $ applyEqualityAt cloc rule equE)


newGame :: IO (IORef AppState)
newGame = do
    (_,_,cursor) <- loadImage "cursor.png"
    (_,_,mushroom) <- loadImage "mushroom.png"
    (_,_,cherry) <- loadImage "cherry.png"
    (_,_,orange) <- loadImage "orange.png"
    (_,_,emptyGreen) <- loadImage "empty_green.png"
    (_,_,emptyRed) <- loadImage "empty_red.png"
    (_,_,emptyPurple) <- loadImage "empty_purple.png"
    (_,_,literal) <- loadImage "literal.png"
    (_,_,eq) <- loadImage "eq.png"
    (_,_,plusbun) <- loadImage "plusbun.png"
    (_,_,mulbun) <- loadImage "mulbun.png"
    (_,_,cat) <- loadImage "cat.png"
    (_,_,frog) <- loadImage "frog.png"
    (_,_,neutral) <- loadImage "neutral.png"
    (_,_,inverse) <- loadImage "neutral.png"

    tomtegebra <- createTextModel AlignCenter
                    "<span font=\"Trebuchet MS 144\">Tomtegebra</span>"
    pressSpace <- createTextModel AlignCenter
                    "<span font=\"Trebuchet MS 48\">Press space to play</span>"
    nextLevel <- createTextModel AlignCenter
                    (  "<span font=\"Trebuchet MS 72\">Level complete!\n</span>"
                    ++ "<span font=\"Trebuchet MS 48\">Press space to continue</span>")
    bothEqual <- createTextModel AlignCenter
                    "<span font=\"Sans 18\">Make both sides equal</span>"
    bindNeutral <- createTextModel AlignCenter
                    "<span font=\"Sans 18\">Reduce one side to the circled animal</span>"
    bindInverse <- createTextModel AlignCenter
                    "<span font=\"Sans 18\">Reduce one side to the circled mushroom</span>"

    newIORef (initGame
        [("bothEqual", bothEqual)
        ,("bindNeutral", bindNeutral)
        ,("bindInverse", bindInverse)
        ,("nextLevel", nextLevel)
        ,("pressSpace", pressSpace)
        ,("title", tomtegebra)]

        [(">", cursor)
        ,("A", mushroom)
        ,("B", cherry)
        ,("C", orange)
        ,("X", emptyGreen)
        ,("Y", emptyRed)
        ,("Z", emptyPurple)
        ,("L", literal)
        ,("E", neutral)
        ,("I", inverse)
        ,("=", eq)
        ,("+", plusbun)
        ,("x", mulbun)
        ,("f", frog)
        ,("o", cat)])
    where loadImage fn = do
            dfn <- getDataFileName fn
            createImageModel dfn

