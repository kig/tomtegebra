{- |
The 'Game' module contains the game state transformations.
-}
module Game (
    AppState(..),
    newGame,
    resetGame,
    nextLevel,
    nextEquation,
    moveCursorLeft,
    moveCursorRight,
    scrollInventoryUp,
    scrollInventoryDown,
    applyCurrentRule
) where
import Graphics.Rendering.OpenGL (GLfloat)
import Algebra
import Models
import Data.IORef
import Graphics.UI.Gtk.Pango.Layout
import Paths_tomtegebra

-- | The game state struct.
data AppState = State {
        equation :: CheckableRule,   -- ^ Current equation
        cursorLocation :: Int,       -- ^ Cursor location in current equation
        inventory :: ProofInventory, -- ^ Proof inventory collected thus far
        inventoryIndex :: Int,       -- ^ Index of the currently selected inventory item
        equationCompleted :: Bool,   -- ^ Is the current equation complete?
        equations :: [CheckableRule],-- ^ Equations left in the current level
        levels :: [Level],           -- ^ Remaining levels to play

        gameOver :: Bool,            -- ^ Game over, show title screen

        texts :: [(String,(Int, Int, Model))], -- ^ Assoc list of text models (including width, height)
        models :: [(String, Model)],           -- ^ Assoc list of equation symbol models
        width :: GLfloat,  -- ^ Current width of the OpenGL window
        height :: GLfloat  -- ^ Current height of the OpenGL window
    }

type Rules = [Rule]

-- | A Level consists of an 'Op' and its defition.
--   The 'Op' is expanded into an Abelian group by 'nextLevel'.
data Level = Level (Op, Rules)


-- | Cat o is a o b = a + b + 1
levelCat = Level ( "o", [(A `o` B) `eq` ((A `plus` B) `plus` Literal 1)] )

-- | Frog f is a f b = a o 1 o b
levelFrog = Level ( "f", [(A `f` B) `eq` ((A `o` Literal 1) `o` B)])
            where f = opf "f"

-- | Bunny x is a x b = 1 f b f a
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
        
        width=600, height=600}

-- | Resets game state to start a new game.
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

-- | Changes to next level.
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

-- | Changes to next equation, or the next level if no equations are left in
--   current level.
nextEquation :: AppState -> AppState
nextEquation sta =
    case equations sta of
        [x] -> nextLevel $ addToInventory x sta
        [] -> nextLevel sta
        x:y:xs -> changeEquation y $ addToInventory x sta {equations = y:xs}

-- | Moves the equation cursor to the left by one step.
moveCursorLeft :: AppState -> AppState
moveCursorLeft = moveCursor (-1)

-- | Moves the equation cursor to the right by one step.
moveCursorRight :: AppState -> AppState
moveCursorRight = moveCursor 1

moveCursor :: Int -> AppState -> AppState
moveCursor amount sta =
    sta { cursorLocation = (cursorLocation sta + amount) `mod` ruleLength (snd $ equation sta) }

-- | Selects the inventory item above the current inventory item.
scrollInventoryUp :: AppState -> AppState
scrollInventoryUp = scrollInventory (-1)

-- | Selects the inventory item below the current inventory item.
scrollInventoryDown :: AppState -> AppState
scrollInventoryDown = scrollInventory 1

scrollInventory :: Int -> AppState -> AppState
scrollInventory amount sta = sta { inventoryIndex = inventoryIndex sta + amount }

addToInventory :: CheckableRule -> AppState -> AppState
addToInventory r sta = sta { inventory = inventory sta ++ [replaceABCwithXYZ $ snd r] }

-- | Applies the currently selected inventory item to the currently selected
--   position in the equation.
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


-- | Creates a new game 'AppState', loading and creating needed textures and models.
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

