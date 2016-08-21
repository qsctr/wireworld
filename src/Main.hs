{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data CellType = EHead | ETail | Conductor deriving (Eq)
data World = World { cells :: [(CellType, (Int, Int))], playing :: Bool, viewPort :: ViewPort }

main :: IO ()
main = do
    mapM_ putStrLn
        [ "Instructions:"
        , "Left click: Add / Remove cell"
        , "Right click: Change cell type"
        , "Space bar: Play/pause"
        , "Arrow keys: Navigate"
        , "Z: Zoom in"
        , "X: Zoom out"
        , "C: Center and reset zoom"
        , "R: Restart game"
        , "Press enter to continue." ]
    _ <- getLine
    play (InWindow "Wireworld" (800, 600) (0, 0)) black 2 initialWorld drawWorld handleInput stepWorld

initialWorld :: World
initialWorld = World { cells = [], playing = False, viewPort = initialViewPort }

initialViewPort :: ViewPort
initialViewPort = viewPortInit { viewPortScale = 15 }

drawWorld :: World -> Picture
drawWorld World {..} = applyViewPortToPicture viewPort $ pictures $ map drawCell cells
    where drawCell (cellType, (x, y)) = color c $ translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1
              where c = case cellType of
                        EHead -> blue
                        ETail -> red
                        Conductor -> yellow

stepWorld :: Float -> World -> World
stepWorld _ world@World { playing = False } = world
stepWorld _ world@World {..} = world { cells = map stepCell cells }
    where stepCell (EHead, pos) = (ETail, pos)
          stepCell (ETail, pos) = (Conductor, pos)
          stepCell (Conductor, (x, y))
              | surroundingEHeadsCount `elem` [1, 2] = (EHead, (x, y))
              | otherwise = (Conductor, (x, y))
              where surroundingEHeadsCount = length $ getEHeadsAt [ (x + w, y + h) | w <- [-1..1], h <- [-1..1] ]
          getEHeadsAt = foldr (\pos acc -> case find ((== pos) . snd) cells of
              Just cell -> if fst cell == EHead then cell : acc else acc
              Nothing -> acc) []

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World {..} = world { playing = not playing }
handleInput (EventKey (MouseButton button) Down _ coords) world@World {..}
    | button == LeftButton = case findCell of
        Just cell -> world { cells = delete cell cells }
        Nothing -> world { cells = (Conductor, pos) : cells }
    | button == RightButton = case findCell of
        Just cell -> world { cells = (nextType $ fst cell, pos) : delete cell cells }
        Nothing -> world
    where findCell = find ((== pos) . snd) cells
          pos = mapPair round $ invertViewPort viewPort coords
          nextType EHead = ETail
          nextType ETail = Conductor
          nextType Conductor = EHead
handleInput (EventKey (SpecialKey key) Down _ _) world@World {..}
    | key == KeyLeft = t (tx + 5, ty)
    | key == KeyRight = t (tx - 5, ty)
    | key == KeyUp = t (tx, ty - 5)
    | key == KeyDown = t (tx, ty + 5)
    where t translation = world { viewPort = viewPort { viewPortTranslate = translation } }
          (tx, ty) = viewPortTranslate viewPort
handleInput (EventKey (Char c) Down _ _) world@World {..}
    | c == 'z' = s $ zoom + 2
    | c == 'x' = s $ max 1 $ zoom - 2
    where s factor = world { viewPort = viewPort { viewPortScale = factor } }
          zoom = viewPortScale viewPort
handleInput (EventKey (Char 'c') Down _ _) world = world { viewPort = initialViewPort }
handleInput (EventKey (Char 'r') Down _ _) _ = initialWorld
handleInput _ world = world

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)