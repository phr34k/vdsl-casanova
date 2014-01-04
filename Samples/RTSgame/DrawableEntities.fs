module RTS.DrawableEntities

open Casanova
open Casanova.Core
open Casanova.Drawing

type [<CasanovaEntity>] Layers = 
      {
        Background      : SpriteLayer
        Planets         : SpriteLayer
        Fleets          : SpriteLayer
        Overlay         : SpriteLayer
        Gui             : SpriteLayer
      }