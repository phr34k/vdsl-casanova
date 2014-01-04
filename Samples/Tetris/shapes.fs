module Shapes

open Microsoft.Xna.Framework.Input
open Casanova
open Casanova.Core
open Casanova.Coroutines
open Casanova.Utilities
open Casanova.Math
open Casanova.Game
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.Core


/// Create the blocks of a random shape, and initialize each block with the create_block function
/// (Block.Create).
let create_random_figure create_block =
    let create (x : int ) (y: int) (color : Color) = 
          create_block (var(Vector2<m>.Create((float32 x) * 50.f<m>,(float32 y) * 50.f<m>)),
                        Vector2<m>.Create(0.f<m>,0.f<m>),
                        color)

    let random_figure =
            match random_interval 0 7 with
            |0 -> let color = Color.Aquamarine
                  seq{ //I
                      yield create 2 0 color
                      yield create 1 0 color
                      yield create 0 0 color
                      yield create -1 0 color
                      }
            |1 -> let color = Color.Orange
                  seq{ //L
                      yield create 1 0 color
                      yield create 0 0 color
                      yield create -1 0 color
                      yield create -1 1 color
                      }
            |2 -> let color = Color.BlueViolet
                  seq{ //J
                      yield create 1 1 color
                      yield create 0 1 color
                      yield create -1 1 color
                      yield create -1 0 color
                      }
            |3 -> let color = Color.Red
                  seq{ //S
                      yield create -1 0 color
                      yield create 0 1 color
                      yield create 0 0 color
                      yield create 1 1 color
                      }
            |4 -> let color = Color.Green
                  seq{ //Z
                      yield create -1 1 color
                      yield create 0 1 color
                      yield create 0 0 color
                      yield create 1 0 color
                      }
            |5 -> let color = Color.Purple
                  seq{ //T
                      yield create 1 0 color
                      yield create -1 0 color
                      yield create 0 1 color
                      yield create 0 0 color
                      }
            |_ -> let color = Color.Yellow
                  seq{ //O
                      yield create 1 1 color
                      yield create 1 0 color
                      yield create 0 1 color
                      yield create 0 0 color
                      }

    random_figure
