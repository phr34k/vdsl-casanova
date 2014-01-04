module PlayingField

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

/// Initialize the borders that delimit the playing field. They will be passed
/// as parameters to the ImmovableBlock.Create function.
let create_borders(canvas : Canvas, figure_dimension : float32<m>) =
    [
      let size = Vector2.Create(50.0f)
      let rotation = 0.0f<rad>
      let origin = Vector2.Create(0.f)
      let path = @"Tetris/prism.png"
      let color = Color(185, 135, 76)

      for x=(-6) to 6 do
        yield ( canvas,
                Vector2.Create((float32 x) * figure_dimension,
                                9.0f * figure_dimension),
                size,
                rotation,
                origin,
                path,
                color)
      for y=(-14) to 9 do
        yield ( canvas,
                Vector2.Create(-6.0f * figure_dimension,
                              (float32 y) * 1.0f * figure_dimension),
                size,
                rotation,
                origin,
                path,
                color)
        yield ( canvas,
                Vector2.Create(6.0f * figure_dimension,
                              (float32 y) * figure_dimension),
                size,
                rotation,
                origin,
                path,
                color)
    ]


