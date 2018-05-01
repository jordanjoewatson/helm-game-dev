module Graphics where

import Maps
import Locations

import Prelude hiding (Right, Left)
import Linear.V2 (V2(V2))

import Data.Fixed
import qualified Data.Map as M

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Engine.SDL as SDL



pausedOverlay :: Bool -> Double -> Double -> Form SDLEngine
pausedOverlay paused x y
  | paused =
      text $ Text.height 30 $
             Text.color (rgb 1 1 1) $
             Text.toText "Game Paused"
  | otherwise = (move (V2 32 32) $ blank)


checkLocation :: Double -> Double -> Double -> Double -> Location
checkLocation x y a b
  | buildingTile == 'm' = House
  | otherwise = World
    where
      x' = (floor ((x + a) / 32)) -- was x+2
      y' = (floor ((y + b) / 32))
      buildingTile = (buildingsMap !! y') !! x'

-- need to add code to make the check function work on current working map
check :: Double -> Double -> Double -> Double -> V2 Double
check x y a b
  | x' >= length (tileMap !! 0) || x' <= 0 = V2 x y
  | y' <= 0 || y' >= length tileMap = V2 x y
  | buildingTile `elem` buildingTiles = V2 x y
  | tile `elem` walkTiles = V2 (x + a) (y + b)
  | otherwise = V2 x y
  where
    x' = (floor ((x + a) / 32)) -- was x+2
    y' = (floor ((y + b) / 32))
    tile = (tileMap !! y') !! x'
    buildingTile = (buildingsMap !! y') !! x'


convertToSprites :: M.Map String (Image SDLEngine) -> [Char] -> [Form SDLEngine]
convertToSprites tiles [] = []
convertToSprites tiles (c:cs) = tile ++ convertToSprites tiles cs
  where
    tile | c == '-' || c == '.' = [(move (V2 32 32) $ blank)]
         | otherwise = [image (V2 32 32) (tiles M.! [c])]

spreadTiles :: [Int] -> [Form SDLEngine] -> [Form SDLEngine]
spreadTiles [] [] = []
spreadTiles (n:ns) (tile:ts) = [(move (V2 (fromIntegral n) 0) $ tile)] ++ spreadTiles ns ts

spreadRows :: Int -> [Form SDLEngine] -> [Form SDLEngine]
spreadRows n [] = []
spreadRows n (t:ts) = [move (V2 0 (n'*32)) $ t] ++ spreadRows n ts
  where
    n' = fromIntegral n

moveMap :: [Form SDLEngine] -> Double -> Double -> [Form SDLEngine]
moveMap [] x y = []
moveMap (t:tiles) x y = [move (V2 (-x) (-y)) $ t] ++ moveMap tiles x y

background :: [[Char]] -> M.Map String (Image SDLEngine) -> Double -> Double -> [Form SDLEngine]
background tileMap tiles x y = b
  where
    rows = [ (convertToSprites tiles row) | row <- tileMap ]
    xs = [ spreadTiles (map (\n -> 32 * n) [ 1 .. length row ]) row | row <- rows ]
    b = moveMap (concat ((zipWith (spreadRows) [ 1 .. length xs ] xs))) x y

-- paint player
player :: M.Map String (Image SDLEngine) -> [Char] -> Int -> Form SDLEngine
player imgs c f = image (V2 32 32) (imgs M.! i)
  where
    i | f == 0 = c
      | f `mod` 8 == 0 = "fighter7"
      | f `mod` 7 == 0 = "fighter6"
      | f `mod` 6 == 0 = "fighter5"
      | f `mod` 5 == 0 = "fighter4"
      | f `mod` 4 == 0 = "fighter3"
      | f `mod` 3 == 0 = "fighter2"
      | f `mod` 2 == 0 = "fighter1"
      | f `mod` 1 == 0  = "fighter0"
      | otherwise = c
