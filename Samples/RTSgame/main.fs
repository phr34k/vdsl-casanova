// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module RTS.Main

open Casanova
open Casanova.Core
open Casanova.Drawing
open Casanova.Game
open Casanova.Math
open Casanova.Coroutines
open Casanova.Action
open RTS.World
open Casanova.Input
open Casanova.Utilities

/// This sample is slightly more complex than the others, and as such it is divided into multiple files.
/// The game logic is in star_system.fs, while selection and turns handling are in world.fs.

let rec start_game (start_selector : StartGameSignature<'a>) (start_game_args:StartGameArgs)  =
  /// We start by extracting the graphics device, the content loader, and the exit function from the start_game_args 
  /// which Casanova passes to start_game.
  let (device,content,exit) = start_game_args.GraphicsDevice,start_game_args.Content,start_game_args.Quit

  /// For scripts we redefine the (!) operator to ignore double buffering for rules.
  let inline (!) x = immediate_lookup x

  /// We initialize the game world with only two players, red and blue. The game world needs
  /// to create visual assets, so it needs the graphics device and the content manager.
  let world = World.Create([|"p1", Color.Blue;"p2", Color.Red|], device, content)

  /// There is no main script in this game, even though an AI or additional map logic
  /// would go here.
  let main = yield_

  let input = 
    /// Climb and scroll speeds control how fast the camera moves.
    let climb_speed = 10.0f<m/s>
    let scroll_speed = 3000.0f<m/s>
    [
      /// We use the wait_scroll_wheel 1 to read all movements of the scroll wheel with absolute value
      /// greater than 1. When the event is ticked, then we modify the camera Climb field which
      /// controls the speed at which the camera moves up or down.
      wait_scroll_wheel 1 ==> fun delta -> co{ world.Camera.Climb := !world.Camera.Climb - climb_speed * delta}

      /// We use wait_key_press k to determine when key k is pressed. When W,A,S, or D is pressed then
      /// we change the camera Velocity field which then moves the camera.
      wait_key_press Keys.W => co{ world.Camera.Velocity := !world.Camera.Velocity + Vector2.UnitY * scroll_speed }
      wait_key_press Keys.S => co{ world.Camera.Velocity := !world.Camera.Velocity - Vector2.UnitY * scroll_speed }
      wait_key_press Keys.A => co{ world.Camera.Velocity := !world.Camera.Velocity + Vector2.UnitX * scroll_speed }
      wait_key_press Keys.D => co{ world.Camera.Velocity := !world.Camera.Velocity - Vector2.UnitX * scroll_speed }
      
      //wait_key_press Keys.C => co{ Casanova.Game.sw.Close() }
      

      /// Pressing Esc closes the game.
      wait_key_press Keys.Escape => start_game_args.PushStack (start_pause_menu start_selector, false)

      /// When the left mouse key is clicked (but not released)
      wait_left_mouse_down =>
        co{ 
          /// First we store the mouse position in layer coordinates in start_selection
          let mx,my = mouse_position()
          let start_selection = world.Layers.Fleets.Unproject(Vector2.Create(mx,my))

          /// update_selection updates the selection rectangle with the current mouse position;
          /// since it repeats at the end, update_selection is a script that would run forever
          let update_selection = 
            co{
              let mx,my = mouse_position()
              let end_selection = world.Layers.Fleets.Unproject(Vector2.Create(mx,my))
              do world.Selection.UpdateSelection(world,start_selection, end_selection)
              do! yield_
            } |> repeat_

          /// We run update_selection and wait_left_mouse_up in parallel; the first 
          /// to terminate will be of course wait_left_mouse_up
          do! wait_left_mouse_up .||> update_selection

          /// We get the final mouse position
          let mx,my = mouse_position()
          let end_selection = world.Layers.Fleets.Unproject(Vector2.Create(mx,my))

          /// We compute the selection area
          let mx = min start_selection.X end_selection.X
          let my = min start_selection.Y end_selection.Y
          let Mx = max start_selection.X end_selection.X
          let My = max start_selection.Y end_selection.Y

          /// We set Selected to true for all fleets that are within the selection rectangle.
          for f in world.StarSystem.Fleets do
            if !f.Owner = !(!world.TurnManager.CurrentTurnOwner) &&
                (mx <= (!f.Position).X && my <= (!f.Position).Y &&
                 Mx >= (!f.Position).X && My >= (!f.Position).Y ||
                 Vector2.Distance(!f.Position, start_selection) <= 30.0f<m>) then
              do f.Selected := true
            else
              do f.Selected := false

          /// We end selection by deleting the selection rectangle.
          do world.Selection.EndSelection()
        }

      /// When backspace is pressed then all fleets are deselected
      wait_key_press Keys.Back => co{ for f in world.StarSystem.Fleets do f.Selected := false }

      /// When the right mouse button is pressed then all selected fleets are sent
      /// to the click location by setting their Target. This event only goes through
      /// if the current player is the owner of the selected fleets.
      wait_right_mouse_press =>
        co{ 
          let mx,my = mouse_position()
          let target = world.Layers.Fleets.Unproject(Vector2.Create(mx,my))
          for f in !world.Selection.SelectedFleets do
            if !(!f).Owner = !(!world.TurnManager.CurrentTurnOwner) then
              do (!f).SetTarget(target)
        }
    ]

  /// start_game returns as output the initial game world, the main script, and the input scripts.
  in world,main,input

// Used to allow menu to run a new game
and start_main_menu (start_selector : StartGameSignature<'a>) =
  MainMenu.start_menu "RTSGame" @"RTS\Backgrounds\background" start_game start_selector

and start_pause_menu (start_selector : StartGameSignature<'a>) =
  PauseMenu.start_menu "RTSGame" @"RTS\Backgrounds\background" start_game start_selector
