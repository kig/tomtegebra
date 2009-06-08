\documentstyle{article}

\begin{document}

\section{Introduction}

Tomtegebra is a most exciting game revolving around groups, rings and fields.

The game is composed of an adventure part where you find new functions
to prove, and the proof part where you reign over functions by showing
them for what they really are!

\begin{code}
module Game where
import Algebra

data AppState = State {
        equation :: Rule,
        cursorLocation :: Int,
        inventory :: ProofInventory,
        inventoryIndex :: Int,
        equationCompleted :: Bool,

        angle :: Double,
        width :: Double,
        height :: Double,
        dir :: Double
    }

initState = State {
                equation = (A `o` (B `o` C)) `eq` ((A `o` B) `o` C),
                cursorLocation = 0,
                inventory = [(A `o` B) `eq` ((A `plus` B) `plus` Literal 1)] ++ (abelianGroup "+"),
                inventoryIndex = 0,
                equationCompleted = False,

                angle=0, width=600, height=600, dir=1.0}

\end{code}

\end{document}
