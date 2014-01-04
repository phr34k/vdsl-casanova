module MenuGameOver

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
    UICanvas        : Canvas
    // The background image of this menu
    Background : Sprite
    // The drawable text with the content
    //    Text      : Text
    Info            : Text
 }


let rec start_menu game_name background start_actual_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) = 
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
      UICanvas       = ui_canvas
      Info = Text.Create(ui_canvas, @"PacMan\arial",Vector2<pixel>.Zero, Vector2<pixel>.Zero, "Game Over",Color.Red,Vector2<pixel>.Create(300.0f))
    }

  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x

  let main = 
   co{ 
      do! wait(3.0f<s>)
      do! args.PopStack 
    } 

  // Four input scripts: 
  // when the user presses F9 key the game will be saved
  // when the user presses F10 key the game will be loaded
  // when the user presses Q key the game end and the main menu is showed
  // when the user presses Escape key the game will be resumed poping the stack
  let input = 
    []
  world, main, input
