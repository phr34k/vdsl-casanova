module RTSgame.DrawableEntities

open Casanova
open Casanova.Drawing

type Layers = 
      {
        Background      : SpriteLayer
        Planets         : SpriteLayer
        Fleets          : SpriteLayer
      }