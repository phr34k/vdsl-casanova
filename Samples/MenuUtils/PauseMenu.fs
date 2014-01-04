module PauseMenu

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

// The pause Menu contains the menu content text, the background image and the layer associated to both.
[<CasanovaWorld>]
type Menu = {
    // The layer used to draw the background
    BackgroundLayer : SpriteLayer
    // The layer used to draw the menu
    TextLayer       : SpriteLayer
    UICanvas      : Canvas
    // The background image of this menu
    Background : Sprite
//    // The drawable text with the content
//    Text      : Text
    SaveButton          : Button
    LoadButton          : Button
    QuitButton          : Button
  }


let start_menu game_name background start_actual_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) = 
  // The text layer
  let layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  // The background layer
  let background_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  let ui_canvas = Canvas.Create(background_layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 1000.f)
  // The menu is created with the two layer the background image and the drawable text 
  let world = 
    {
      TextLayer = layer
      BackgroundLayer = background_layer
      Background = 
        Sprite.Create(
          background_layer,
          -Vector2<_>.One * 500.f,
          Vector2<_>.One * 4000.f, 
          0.f<_>,
          Vector2<_>.Zero,
          @"UI/white_pixel",
          Color.FromNonPremultiplied(1,1,1,150))
      UICanvas          = ui_canvas
      SaveButton        = Button.Create(ui_canvas, Vector2<pixel>.UnitY * -100.f, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Save game", Color.White, 10.f<pixel>)
      LoadButton        = Button.Create(ui_canvas, Vector2<pixel>.Zero, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Load game", Color.White, 10.f<pixel>)
      QuitButton        = Button.Create(ui_canvas, Vector2<pixel>.UnitY * 100.f, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Quit", Color.White, 10.f<pixel>)
    }

  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x

  do Coroutines.run_script (wait_condition (fun () -> !world.SaveButton.IsSelected)   => args.Save game_name)
  do Coroutines.run_script (wait_condition (fun () -> !world.LoadButton.IsSelected)   => args.Load game_name)
  do Coroutines.run_script (wait_condition (fun () -> !world.QuitButton.IsSelected)   => args.SetStack(MainMenu.start_menu game_name background start_actual_game start_selector))
  
  // The main script does nothing.
  let main = 
    co{ do! yield_} 

  // Four input scripts: 
  // when the user presses F9 key the game will be saved
  // when the user presses F10 key the game will be loaded
  // when the user presses Q key the game end and the main menu is showed
  // when the user presses Escape key the game will be resumed poping the stack
  let input = 
    [
      wait_key_press Keys.Escape  => args.PopStack 
    ]
  world, main, input




