module RenderGame where
import Game
import Algebra

drawVictory :: IO ()
drawVictory = putStrLn "VICTORY!!! HUZZAH!!! BANZAI!!!"

drawLevel :: AppState -> IO ()
drawLevel st =
    let equ = equation st
        cloc = cursorLocation st
        inv = inventory st
        invIdx = inventoryIndex st in do
    drawInventory (inventoryFor cloc equ inv) invIdx
    drawEquation equ cloc

drawEquation :: CheckableRule -> Int -> IO ()
drawEquation (_,rule) idx = drawExpr (toExpr rule) idx

drawExpr :: Expr -> Int -> IO ()
drawExpr expr idx = do
    putStrLn "\nEquation:"
    putStrLn $ show expr
    putStrLn $ show (subExprAt idx expr)

drawInventory :: ProofInventory -> Int -> IO ()
drawInventory inventory idx =
    let idx' = idx `mod` length inventory in do
    putStrLn "\nInventory:"
    mapM_ (putStrLn.show) $ take idx' inventory
    putStrLn $ "--" ++ show (inventory !! idx')
    mapM_ (putStrLn.show) $ drop (idx'+1) inventory
