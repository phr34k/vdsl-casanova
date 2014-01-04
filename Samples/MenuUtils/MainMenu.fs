module MainMenu

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

// The Menu contains the menu content text, the background, the layer used to draw the background amd the layer used for the text.
[<CasanovaWorld>]
type Menu = {
    // The layer used to draw the background
    BackgroundLayer : SpriteLayer
    // The layer used to draw the menu
    TextLayer     : SpriteLayer
    UICanvas      : Canvas
    // The background image of this menu
    Background : Sprite
//    // The drawable text with the content
//    Text      : Text
    NewGameButton      : Button
    LoadButton         : Button
    QuitButton         : Button
  }


let start_menu game_name background start_actual_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) = 
  // The layer for the text
  let layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  // The layer for the background
  let background_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  let ui_canvas = Canvas.Create(background_layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 1000.f)
  // The menu is created with the two layer the background image and the drawable text 
  let world = 
    {
      TextLayer = layer
      BackgroundLayer = background_layer
      UICanvas = ui_canvas
      Background = 
        Sprite.Create(background_layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 2000.0f, background)
//      Text = 
//        Text.Create(
//          layer,
//          "Arial",
//          -Vector2<_>.UnitX * 450.f,
//          -Vector2<_>.One * 0.5f,
//          "Start new game (N)\nLoad (F10)\nQuit (Q)",
//          Color.White,
//          Vector2<_>.One * 120.f)
      NewGameButton     = Button.Create(ui_canvas, Vector2<pixel>.UnitY * -100.f, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "New game", Color.White, 10.f<pixel>)
      LoadButton        = Button.Create(ui_canvas, Vector2<pixel>.Zero, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Load game", Color.White, 10.f<pixel>)
      QuitButton        = Button.Create(ui_canvas, Vector2<pixel>.UnitY * 100.f, Vector2<pixel>(300.f<pixel>, 75.f<pixel>), "Quit", Color.White, 10.f<pixel>)
    }

  do Coroutines.run_script (wait_condition (fun () -> !world.NewGameButton.IsSelected)  => args.SetStack (start_actual_game start_selector))
  do Coroutines.run_script (wait_condition (fun () -> !world.LoadButton.IsSelected)     => args.Load game_name)
  do Coroutines.run_script (wait_condition (fun () -> !world.QuitButton.IsSelected)     => args.PushStack(ConfirmationMenu.start_menu (args.SetStack start_selector), true))

  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x
  
  // The main script does nothing.
  let main = 
    co{ do! yield_}

  // Three input scripts: 
  // when the user presses F10 key the game will be loaded
  // when the user presses N key a new game wil start
  // when the user presses Q key it quit
  let input = []
  world, main, input