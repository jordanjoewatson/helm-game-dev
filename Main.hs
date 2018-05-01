{-# LANGUAGE RecordWildCards #-}

import Maps
import Graphics
import Image
import Logic
import Locations

import System.Directory

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

-- data Location = World
--               | House deriving Eq

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
  | Space
  | Pause


data Model = Model
  { player_pos :: V2 Double
  , health :: Int
  , right :: Bool
  , left :: Bool
  , up :: Bool
  , down :: Bool
  , fight :: Int
  , direction :: [Char]
  , paused :: Bool
  , location :: Location
  }

square_size = 32

initial :: (Model, Cmd SDLEngine Action)
initial =
  (Model
    { player_pos = V2 96 96 -- was 32 32
    , health = 100
    , right = False
    , left = False
    , up = False
    , down = False
    , fight = 0
    , direction = "x"
    , paused = False
    , location = World
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

update model@Model { .. } Space
  | paused = (model, Cmd.none)
  | fight == 0 =
    (model
      { fight = 16
      }, Cmd.none)
  | otherwise = (model, Cmd.none)

update model@Model { .. } Pause =
  (model
    { paused = play
    }, Cmd.none)
  where
    play | paused = False
         | otherwise = True

-- Update player position based on the keys being pressed
update model@Model { .. } (Animate dt)
  | paused = (model, Cmd.none)
  | right && up =
    (model
      { player_pos = check x y 2 (-2)
      , fight = fight'
      , direction = "e"
      }, Cmd.none)
  | right && down =
    (model
      { player_pos = check x y 2 2
      , fight = fight'
      , direction = "c"
      }, Cmd.none)
  | right =
    (model
      { player_pos = check x y 2 0
      , fight = fight'
      , direction = "d"
      }, Cmd.none)
  | left && up =
    (model
      { player_pos = check x y (-2) (-2)
      , fight = fight'
      , direction = "q"
      }, Cmd.none)
  | left && down =
    (model
      { player_pos = check x y (-2) 2
      , fight = fight'
      , direction = "z"
      }, Cmd.none)
  | left =
    (model
      { player_pos = check x y (-2) 0
      , fight = fight'
      , direction = "a"
      }, Cmd.none)
  | up =
    (model
      { player_pos = check x y 0 (-2) -- change back to y - 2
      , location = checkLocation x y 0 (-2)
      , fight = fight'
      , direction = "w"
      }, Cmd.none)
  | down =
    (model
      { player_pos = check x y 0 2
      , fight = fight'
      , direction = "x"
      }, Cmd.none)
  | otherwise =
    (model
      { direction = "x"
      , fight = fight'
      }, Cmd.none)
    where
      V2 x y = player_pos
      fight' | fight > 0 = fight - 1
             | otherwise = 0


update model@Model { .. } AllStop =
  (model, Cmd.none)


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
  , Keyboard.presses $ \key -> (case key of
    Keyboard.SpaceKey -> Space
    Keyboard.EscapeKey -> Pause
    _ -> AllStop)
  , Time.fps 60 Animate
  ]

t :: Eq a => a -> [[Char]]
t x = tileMap

-- can pass in current direction into player function to print correct direction
view :: M.Map String (Image SDLEngine) -> Model -> Graphics SDLEngine
view imgs model@Model { .. } = Graphics2D $
  center (V2 (500 / 2) (500 / 2)) $ collage
    (  background backgroundMap imgs x y
    ++ [ move (V2 0 0) $ player imgs direction fight ]
    ++ background objectMap imgs x y
    ++ [ move (V2 100 100) $ pausedOverlay paused x y ])
    where
      V2 x y = player_pos
      menu = pausedOverlay paused x y
      backgroundMap | location == World = tileMap
                    | otherwise = houseMap
      objectMap | location == World = buildingsMap
                | otherwise = houseObjects

-- idea for view function
{-|

backgroundMap | location == World = tileMap
              | otherwise = houseMap
Instead of having background
player
buildings
paused overlay

could have in the where,
overallBackground = group $ [
  background
  ...
]
or as well as that
have a boolean value representing areas of the map
backgroundMap | zoneOne = background tileMap img x y ...
              | houseOne = background house img x y ...

and when entering different zones then change the boolean values
-}





main :: IO ()
main = do
  engine <- SDL.startup

  let imgs = imagePaths
      loadAssets' [] game loaded = game loaded
      loadAssets' ((f,id):fs) game loaded = do
        SDL.withImage engine f $ \image ->
          loadAssets' fs game (M.insert id image loaded)
      loadAssets files game = loadAssets' files game M.empty

  loadAssets imgs $ \allAssets ->
    run engine GameConfig
      { initialFn       = initial
      , updateFn        = update
      , subscriptionsFn = subscriptions
      , viewFn          = view allAssets
      }

-- run engine GameConfig
