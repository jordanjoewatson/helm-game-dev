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

grid_map = [ "0788888885555588890"
           , "0000000045555600000"
           , "0000000045555600000"
           , "0000000045555600000"
           , "0000000045555600000"
           , "0000122255555522230"
           , "0000455555555555560"
           , "00004555@£$55555560"
           , "00004555%^&55555560"
           , "00004555*()55555560"
           , "0000455555555555560"
           , "0000455555555555560"
           , "0000455555555555560"
           , "0000788888885588890"
           , "0000000000004600000"
           , "0000000000007900000"
           , "0000000000000000000" ]

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
player :: M.Map String (Image SDLEngine) -> [Char] -> Form SDLEngine
player imgs c = image (V2 32 32) (imgs M.! c)
--  | n == 1 = image (V2 32 40) (imgs M.! "w")
--  | n == 2 = image (V2 32 40) (imgs M.! "d")
--  | n == 3 = image (V2 32 40) (imgs M.! "s")
--  | n == 4 = image (V2 32 40) (imgs M.! "a")
--   | otherwise = image (V2 40 40) (imgs M.! "s")
-- player imgs n = (move (V2 160 (50)) $ filled (rgb 1 1 1) $ square 32)
