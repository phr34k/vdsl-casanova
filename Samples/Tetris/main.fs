module Tetris

(*
IMPORTANT TODO:
  * document the world entity
  * actual_position appears in two very different places: Input and Entities; should it then not be a static method of the game world?
  * ImmovableBlock.Create takes *way* too many parameters; reduce them to the bare minimum and use a better overload for Sprite.Create
  * Block.Create uses an inappropriate overload for Sprite.Create
      moreover, why two different parameters for the position and the relative position?
  * why is Block.RelativePosition a Var<Vector2>?
  * Vector2.Create is often used very wrongly: Vector2.Create((!b.RelativePosition).X,(!b.RelativePosition).Y) is a pointless way of saying !b.RelativePosition
  * repeated constants (for example FigureDimension, or 50.0f<m>, which appears everywhere) in a separate file

SIGNIFICANT IMPROVEMENTS TODO:
  * the Figure is a CasanovaWorld, not a CasanovaEntity, so its blocks can access it without going through the game world
  * use a menu for the "You lose!" screen
  * Dimension should be changed into Size at every occurrence

SECONDARY IMPROVEMENTS TODO:
  * why does the main script perform no logic? I would expect it to add the fallen figure to the immovable blocks, respawn the figure, and then generate a new one...
      moreover, the semantics of world.Timer are very unclear
*)


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
open Entities
open Input


/// Initialize the game world, the main script, and the input scripts.
let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
  let world =
    /// The main canvas of the game is used to ensure that the scene is drawn in an area of the screen with no stretching.
    let canvas = Canvas.Create(args.DefaultLayer, Vector2<pixel>.Zero, Vector2<pixel>.One * 800.f, StretchMode.Uniform)

    /// Size of a block.
    let figure_dimension = 50.0f<m>

    /// Initialize the (initially empty) figure. It will be filled with blocks and will
    /// fall and then be reset throughout the whole duration of the game.
    let figure = {
      Dimension         = figure_dimension
      Center            = Rule.Create (Vector2<m>.Create(0.0f<m>, 0.0f<m>))
      Blocks            = RuleList.Create (fun () -> Seq.empty)
      Color             = Color.White
    }

    /// Initialize the borders of the game, which ensure that the falling figure cannot 
    /// leave the playing field.
    let borders = 
      [ for border in PlayingField.create_borders(canvas, figure_dimension) do
          yield ImmovableBlock.Create border ]

    /// Initialize the game world and its textures. The initial figure, accumulated blocks, score,
    /// etc. are all zeroed out.
    { 
      Canvas            = canvas
      Background        = Sprite.Create(args.DefaultLayer, Vector2.Create(0.0f, 0.0f), Vector2.Create(1000.0f, 1000.0f), 0.0f<rad>, Vector2.Create(0.0f), @"Tetris/city2.png", Color.White, true)
      ScoreAppereance   = Text.Create (canvas, Vector2.Create(-480.0f, -480.0f), Vector2<pixel>.Create(400.0f, 200.0f))

      GameInfo          = Text.Create (canvas, Vector2.Create(0.0f, 0.0f), Vector2.Create(100.0f, 50.0f))
      Timer             = Rule.Create 0.0f<s>
      Movable_Figure    = figure
      Immovable_Blocks  = RuleList.Create (fun () -> Seq.empty)
      Will_Collide      = Rule.Create false
      Borders           = borders
      Score             = Rule.Create 0.0f
      Difficulty        = Rule.Create 0.0f
    }

  /// For scripts we redefine the (!) operator to ignore double buffering for rules.
  /// This line must precede any script declaration.
  let inline (!) x = immediate_lookup x

  /// The main script does nothing.
  let main =  yield_

  /// Initialize the input scripts. These will invoke the pause menu, so as a parameter
  /// to get_input_scripts we must also pass the function that sets it.
  let input = Input.get_input_scripts (world, args, start_pause_menu start_selector)

  world, main, input


/// Simply invoke the PauseMenu.start_menu function; pass it the name of the game, 
/// the right background to use, and the start_game function which allows the
/// pause menu to restart the game.
and start_pause_menu(start_selector : StartGameSignature<'a>) =
  PauseMenu.start_menu "Tetris" @"Tetris\city2.png" start_game start_selector
