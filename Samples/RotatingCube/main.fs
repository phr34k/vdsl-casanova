module RotatingCube

open Casanova
open Casanova.Input
open Casanova.Core
open Casanova.Coroutines
open Casanova.Utilities
open Casanova.Math
open Casanova.Game
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.Core
open Casanova.StandardLibrary.Physics

/// The game world contains a layer for the models, a 
/// 3D camera for the transformation matrices that go
/// into the layer, a model, and its pitch and roll as
/// set by the user input
type [<CasanovaWorld>] World = {
  Models      : ModelLayer
  Camera      : Camera3D
  Cube        : Model
  Pitch       : Rule<float32<rad/s>>
  Roll        : Rule<float32<rad/s>>
} with 
  /// The cube rotates along the Y axis, plus it incorporates X and Z rotations
  /// according to user input.
  static member CubeRotation'(self:World, dt:float32<s>) =
    !self.Cube.Rotation * Microsoft.Xna.Framework.Quaternion.CreateFromYawPitchRoll(dt * 0.1f<_>, dt * !self.Pitch * 0.1f<_>, dt * !self.Roll * 0.1f<_>)
  /// Reset user input rotations
  static member Pitch'(self:World, dt:float32<s>) = 0.0f<rad/s>
  static member Roll'(self:World, dt:float32<s>) = 0.0f<rad/s>
  /// Set the view and projection matrices of the models layer from the camera.
  static member ModelsView'(self:World, dt:float32<s>) = self.Camera.View
  static member ModelsProjection'(self:World, dt:float32<s>) = self.Camera.Projection


let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
  /// Initialize the model layer with the graphics device and the content manager.
  let model_layer = ModelLayer.Create(args.GraphicsDevice, args.Content)

  /// Setup the observer on (0,0,100)
  let camera = Camera3D.Create(Vector3.UnitZ * 100.0f + Vector3.UnitY * 2.0f, args.GraphicsDevice.Viewport.AspectRatio)

  /// The cube is at the origin, 30x30x30 in size, and loaded from the "cube" file.
  let cube = Model.Create(model_layer, Vector3.Zero, Vector3.One * 30.0f, Microsoft.Xna.Framework.Quaternion.Identity, @"3D Test\cube")

  let world =
    {
      Models      = model_layer
      Camera      = camera
      Cube        = cube
      Pitch       = Rule.Create 0.0f<rad/s>
      Roll        = Rule.Create 0.0f<rad/s>
    }

  // Internal requirement. Leave *before* scripts.
  let inline (!) x = immediate_lookup x

  /// The main script does nothing
  let main = yield_

  do hide_mouse_cursor()

  // when the user presses the F9 key the game will be saved in RotatingCube file;
  // when the user presses the F10 key the game will be loaded from RotatingCube file;
  // when the user presses an arrow key the cube rotates
  // when the user presses WASD the camera moves
  // when the user moves the mouse the camera turns
  // when the user presses the escape key the game will be closed and the main menu will open.
  let input =
    [
      wait_key_press Keys.F9      => args.Save("RotatingCube");
      wait_key_press Keys.F10     => args.Load("RotatingCube");
      wait_key_down  Keys.Up      => co{ return world.Pitch := 100.0f<rad/s> }
      wait_key_down  Keys.Down    => co{ return world.Pitch := -100.0f<rad/s> }
      wait_key_down  Keys.Left    => co{ return world.Roll := 100.0f<rad/s> }
      wait_key_down  Keys.Right   => co{ return world.Roll := -100.0f<rad/s> }

      wait_mouse_move =>> 
        fun (mx,my) ->
          co{
            do world.Camera.Rotation := Vector2.Create(mx,my) * 0.1f<rad/s>
            do set_mouse_position(200,200)
          };

      wait_key_down  Keys.W       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Forward * 30.0f<m/s> }
      wait_key_down  Keys.A       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Left * 30.0f<m/s> }
      wait_key_down  Keys.S       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Backward * 30.0f<m/s> }
      wait_key_down  Keys.D       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Right * 30.0f<m/s> }
      wait_key_press Keys.Escape  => 
        co{
          do show_mouse_cursor()
          do! args.SetStack start_selector
        }

    ]
  world, main, input

