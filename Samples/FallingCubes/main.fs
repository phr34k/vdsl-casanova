module FallingCubes

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
/// into the layer, a series of (falling) cubes and
/// a static floor.
type [<CasanovaWorld>] World = {
  Physics     : PhysicsWorld3D
  ModelLayer  : ModelLayer
  Camera      : Camera3D
  Cubes       : Var<List<Cube>>
  Floor       : PhysicsEntity3D
} with 
  /// Set the view and projection matrices of the models layer from the camera.
  static member ModelLayerView'(self:World, dt:float32<s>) = self.Camera.View
  static member ModelLayerProjection'(self:World, dt:float32<s>) = self.Camera.Projection

/// A cube simply contains a physics entity and a drawable model.
and [<CasanovaEntity>] Cube = {
  Physics     : PhysicsEntity3D
  Model       : Model
} with
  /// The model position and rotation come from the physics simulation.
  static member ModelPosition'(self:Cube, dt:float32<s>) = self.Physics.Position
  static member ModelRotation'(self:Cube, dt:float32<s>) = self.Physics.Orientation

let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
  /// Initialize the model layer with the graphics device and the content manager.
  let model_layer = ModelLayer.Create(args.GraphicsDevice, args.Content)

  /// Setup the physics world. By default gravity is (0,-9.81,0)
  let physics = PhysicsWorld3D.Create()

  /// Setup the observer on (0,0,100)
  let camera = Camera3D.Create(Vector3.UnitZ * 100.0f, args.GraphicsDevice.Viewport.AspectRatio)

  /// Create a randomly positioned cube.
  let create_cube (i:int) =
    let x = random_range -30.0f<m> 30.0f<m>
    let y = (float32 (i%10)) * 10.0f<m>
    let z = random_range -30.0f<m> 30.0f<m>
    {
      // The cube is dynamic, that is it moves, and gets pushed around by other entities.
      Physics     = physics.CreateBox(x, y, z, 10.0f<m>, 10.0f<m>, 10.0f<m>, 10.0f<kg>, true)
      /// The cube model is 10x10x10 in size, and loaded from the "cube" file.
      Model       = Model.Create(model_layer, Vector3.Create(x, y, z), Vector3.One * 10.0f, Microsoft.Xna.Framework.Quaternion.Identity, @"3D Test\cube")
    }
    

  /// We create 31 boxes that will fall from the sky. To avoid letting them fall indefinitely, we create
  /// a non-dynamic floor which is very wide.
  let world =
    {
      Physics     = physics
      ModelLayer  = model_layer
      Camera      = camera
      Cubes       = var [for i = 0 to 30 do yield create_cube i]
      Floor       = physics.CreateBox(0.0f<m>, -20.0f<m>, 0.0f<m>, 1000.0f<m>, 10.0f<m>, 1000.0f<m>, 0.0f<kg>, false)
    }

  // Internal requirement. Leave *before* scripts.
  let inline (!) x = immediate_lookup x

  /// The main script does nothing
  let main = yield_

  do hide_mouse_cursor()

  // when the user presses the F9 key the game will be saved in the FallingCubes file;
  // when the user presses the F10 key the game will be loaded from the FallingCubes file;
  // when the user presses space new cubes appear
  // when the user presses WASD the camera moves
  // when the user moves the mouse the camera turns
  // when the user presses the escape key the game will be closed and the main menu will open.
  let input =
    [
      wait_key_press Keys.F9      => args.Save("FallingCubes");
      wait_key_press Keys.F10     =>
          co{
            do set_mouse_position(200,200)
            do! args.Load("FallingCubes");
            do set_mouse_position(200,200)
          }

      wait_mouse_move =>> 
        fun (mx,my) ->
          co{
            do world.Camera.Rotation := Vector2.Create(mx,my) * 0.1f<rad/s>
            do set_mouse_position(200,200)
          };

      wait_key_down Keys.Space => 
        co{
          for i = 0 to 10 do world.Cubes := create_cube 9 :: !world.Cubes
          do! wait_key_up Keys.Space .||> wait 0.2f<s>
        }

      wait_key_down  Keys.W       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Forward * 100.0f<m/s> }
      wait_key_down  Keys.A       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Left * 100.0f<m/s> }
      wait_key_down  Keys.S       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Backward * 100.0f<m/s> }
      wait_key_down  Keys.D       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Right * 100.0f<m/s> }
      wait_key_press Keys.Escape  => 
        co{
          do show_mouse_cursor()
          do! args.SetStack start_selector
        }

    ]
  world, main, input
