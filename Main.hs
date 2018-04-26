{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (Right, Left)
import Linear.V2 (V2(V2))

import Data.Fixed
import qualified Data.Map as M

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Mouse as Mouse
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Time as Time
import qualified Helm.Sub as Sub

data Tile = Grass
          | None

data Action
  = Idle
  | Animate Double
  | Right
  | Left
  | Up
  | Down
  | RightStop
  | LeftStop
  | DownStop
  | UpStop
  | AllStop


data Model = Model
  { player_pos :: V2 Double
  , right :: Bool
  , left :: Bool
  , up :: Bool
  , down :: Bool
  }

square_size = 32

initial :: (Model, Cmd SDLEngine Action)
initial =
  (Model
    { player_pos = V2 100 100
    , right = False
    , left = False
    , up = False
    , down = False
    }, Cmd.none)

-- If nothing/Idle, do nothing
update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { .. } Idle = (model, Cmd.none)

-- When button b is pressed, set booleans to allow player to move
update model@Model { .. } Right =
  (model
    { right = True
    , left = False
    }, Cmd.none)
update model@Model { .. } Left =
  (model
    { left = True
    , right = False
    }, Cmd.none)
update model@Model { .. } Up =
  (model
    { up = True
    , down = False
    }, Cmd.none)
update model@Model { .. } Down =
  (model
    { down = True
    , up = False
    }, Cmd.none)

-- tells program to stop moving in direction d when button b is unclicked
update model@Model { .. } RightStop =
  (model
    { right = False
    }, Cmd.none)
update model@Model { .. } LeftStop =
  (model
    { left = False
    }, Cmd.none)
update model@Model { .. } UpStop =
  (model
    { up = False
    }, Cmd.none)
update model@Model { .. } DownStop =
  (model
    { down = False
    }, Cmd.none)


-- Update player position based on the keys being pressed
update model@Model { .. } (Animate dt)
  | right && up =
    (model
      { player_pos = V2 (x + 2) (y - 2)
      }, Cmd.none)
  | right && down =
    (model
      { player_pos = V2 (x + 2) (y + 2)
      }, Cmd.none)
  | right =
    (model
      { player_pos = V2 (x + 2) y
      }, Cmd.none)
  | left && up =
    (model
      { player_pos = V2 (x - 2) (y - 2)
      }, Cmd.none)
  | left && down =
    (model
      { player_pos = V2 (x - 2) (y + 2)
      }, Cmd.none)
  | left =
    (model
      { player_pos = V2 (x - 2) y
      }, Cmd.none)
  | up =
    (model
      { player_pos = V2 x (y - 2)
      }, Cmd.none)
  | down =
    (model
      { player_pos = V2 x (y + 2)
      }, Cmd.none)
  | otherwise = (model, Cmd.none)
    where
      V2 x y = player_pos

update model@Model { .. } AllStop =
  (model
    { left = False
    , right = False
    , up = False
    , down = False
    }, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Keyboard.downs $ \key -> (case key of
    Keyboard.LeftKey -> Left
    Keyboard.UpKey -> Up
    Keyboard.DownKey -> Down
    Keyboard.RightKey -> Right
    _ -> AllStop)
  , Keyboard.ups $ \key -> (case key of
    Keyboard.LeftKey -> LeftStop
    Keyboard.RightKey -> RightStop
    Keyboard.UpKey -> UpStop
    Keyboard.DownKey -> DownStop
    _ -> AllStop)
  , Time.fps 60 Animate
  ]

grid_map = [ "1111100000011111"
           , "1111100000011111"
           , "1111100000011111"
           , "1111100000011111"
           , "1000000000000001"
           , "1000000000000001"
           , "1000000000000001"
           , "1000000000000001"
           , "1111111110011111"
           , "1122222222222211"
           , "1122222222222211" ]

convertToSprites :: [Char] -> [Form SDLEngine]
convertToSprites [] = []
convertToSprites (c:cs)
  | c == '0' = [filled (rgb 0.4 1 0.43) $ square 32] ++ convertToSprites cs
  | c == '1' = [filled (rgb 0.65 0.5 0.2) $ square 32] ++ convertToSprites cs
  | c == '2' = [filled (rgb 0 0 0.6) $ square 32] ++ convertToSprites cs
  | otherwise = [filled (rgb 1 0.5 0.8) $ square 32] ++ convertToSprites cs

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

reduce :: [[Form SDLEngine]] -> [Form SDLEngine]
reduce [] = []
reduce (l:ls) = l ++ reduce ls

background :: Double -> Double -> [Form SDLEngine]
background x y = b
  where
    rows = [ (convertToSprites row) | row <- grid_map ]
    xs = [ spreadTiles (map (\n -> 32 * n) [ 1 .. length row ]) row | row <- rows ]
    b = moveMap (concat ((zipWith (spreadRows) [ 1 .. length xs ] xs))) x y

-- paint player
player :: Form SDLEngine
player = (move (V2 160 (50)) $ filled (rgb 1 1 1) $ square 32)

view :: Model -> Graphics SDLEngine
view model@Model { .. } = Graphics2D $
  center (V2 (500 / 2) (500 / 2)) $ collage
    ( background x y
    ++ [ player
       ])
    where
      V2 x y = player_pos

main :: IO ()
main = do
  engine <- SDL.startup
  run engine GameConfig
    {
      initialFn       = initial,
      updateFn        = update,
      subscriptionsFn = subscriptions,
      viewFn          = view
    }
