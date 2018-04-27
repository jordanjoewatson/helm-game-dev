module Graphics where
import Prelude hiding (Right, Left)
import Linear.V2 (V2(V2))

import Data.Fixed
import qualified Data.Map as M

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Engine.SDL as SDL

grid_map = [ "07888888855555888900000"
           , "00000000455556000000000"
           , "00000000455556000000000"
           , "00000000455556000000000"
           , "00000000455556000000000"
           , "00001222555555222300000"
           , "00004555555555555600000"
           , "00004555@£$555555600022"
           , "00004555%^&555555522258"
           , "00004555*()555555588890"
           , "00004555555555555600000"
           , "00004555555555555600000"
           , "00004555555555555600000"
           , "00007888888855888900000"
           , "00000000000046000000000"
           , "00000000000079000000000"
           , "00000000000000000000000" ]

convertToSprites :: M.Map String (Image SDLEngine) -> [Char] -> [Form SDLEngine]
convertToSprites tiles [] = []
convertToSprites tiles (c:cs) = [image (V2 32 32) (tiles M.! [c])] ++ convertToSprites tiles cs


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

background :: M.Map String (Image SDLEngine) -> Double -> Double -> [Form SDLEngine]
background tiles x y = b
  where
    rows = [ (convertToSprites tiles row) | row <- grid_map ]
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
