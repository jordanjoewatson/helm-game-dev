{-# LANGUAGE RecordWildCards #-}

import Maps
import Graphics
import Image
import Logic

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


data Model = Model
  { player_pos :: V2 Double
  , right :: Bool
  , left :: Bool
  , up :: Bool
  , down :: Bool
  , fight :: Int
  , direction :: [Char]
  }

square_size = 32

initial :: (Model, Cmd SDLEngine Action)
initial =
  (Model
    { player_pos = V2 32 32
    , right = False
    , left = False
    , up = False
    , down = False
    , fight = 0
    , direction = "x"
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
  | fight == 0 =
    (model
      { fight = 16
      }, Cmd.none)
  | otherwise = (model, Cmd.none)

-- Update player position based on the keys being pressed
update model@Model { .. } (Animate dt)
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
    _ -> AllStop)
  , Time.fps 60 Animate
  ]

-- can pass in current direction into player function to print correct direction
view :: M.Map String (Image SDLEngine) -> Model -> Graphics SDLEngine
view imgs model@Model { .. } = Graphics2D $
  center (V2 (500 / 2) (500 / 2)) $ collage
    (  background tileMap imgs x y
    ++ [ move (V2 0 0) $ player imgs direction fight
       ]
    ++ background buildingsMap imgs x y)
    where
      V2 x y = player_pos



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
