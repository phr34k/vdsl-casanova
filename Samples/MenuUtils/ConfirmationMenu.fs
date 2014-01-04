module ConfirmationMenu

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
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
open Casanova.StandardLibrary.UI.Controls

// The Menu contains the text to draw and the associated layer.
[<CasanovaWorld>]
type Menu = {
    // The layer used to draw the menu
    Layer           : SpriteLayer
    Canvas          : Canvas
    ConfirmButton   : Button
    CancelButton    : Button
    // The drawable text to be displayed
    Text      : Text
  }


let start_menu action (args : StartGameArgs) = 
  // The layer
  let layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  let canvas = Canvas.Create(layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 1000.f)
  // The menu is created with the layer and with the drawable text
  let world = 
    {
      Layer = layer
      Canvas = canvas
      Text = 
        Text.Create(
          layer,
          "Segoe",
          -Vector2<_>.UnitY * 400.f,
          "Confirm?",
          Vector2<_>.One * 100.f)
      ConfirmButton = Button.Create(canvas, Vector2<pixel>.UnitY * -60.f, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Ok", Color.White, 10.f<pixel>)
      CancelButton  = Button.Create(canvas, Vector2<pixel>.UnitY * 60.f, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Cancel", Color.White, 10.f<pixel>)
   }
  
  do Coroutines.run_script (wait_condition (fun () -> !world.ConfirmButton.IsSelected) => action)
  do Coroutines.run_script (wait_condition (fun () -> !world.CancelButton.IsSelected)  => args.PopStack)

  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x
  
  // The main script does nothing.
  let main = yield_

  // Three input scripts: 
  // when the user presses the Y key the menu will invoke exit method that closes the application;
  // when the user presses the N or Escape key the menu will be pop the stack removing itself from it;
  let input = 
    [
      wait_key_press Keys.Escape  => args.PopStack
    ]
  world, main, input


