module SnowParticles

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
  Sprites     : SpriteLayer
  Camera      : Camera3D
  Snowflakes  : Snowflakes
} with 
  /// ...
  static member SpritesView'(self:World, dt:float32<s>) = self.Camera.View
  static member SpritesProjection'(self:World, dt:float32<s>) = self.Camera.Projection

and [<CasanovaEntity>] Snowflake = {
  Sprite            : Sprite3D
  Velocity          : Rule<Vector3<m/s>>
} with
  member this.Position = !this.Sprite.Position
  static member SpritePosition'(self:Snowflake,dt:float32<s>) =
    !self.Sprite.Position - Vector3<m/s>.UnitY * dt * 0.1f + !self.Velocity * dt * 0.1f
  static member Velocity'(self:Snowflake,dt:float32<s>) =
    !self.Velocity + Vector3<m/s>.Create(random_range -0.1f 0.1f, random_range -0.1f 0.1f, random_range -0.1f 0.1f)
  static member SpriteRotation'(world:World,self:Snowflake,dt:float32<s>) =
    Quaternion.CreateFromRotationMatrix(Matrix.CreateConstrainedBillboard(self.Position.ToXNA, (!world.Camera.Position).ToXNA, Vector3.UnitY.ToXNA, System.Nullable(world.Camera.Forward.ToXNA), System.Nullable()))
  static member SpriteColor'(world:World, self:Snowflake, dt:float32<s>) =
    Color.White * (lerp (Vector3<m>.Distance(self.Position, !world.Camera.Position) / 20.0f<m>) 0.0f 1.0f)
  static member Create(camera:Camera3D, sprite_layer:SpriteLayer) =
    let snow_field = 20.0f
    let offset = Vector3<m>.Create(random_range -snow_field snow_field, random_range (-snow_field * 0.5f) (snow_field * 0.5f), random_range -snow_field snow_field)
    {
      Sprite    = Sprite3D.Create(sprite_layer, !camera.Position + offset, Quaternion.Identity, Vector2<m>.One * 0.1f, Color.White, @"Particles\snowflake")
      Velocity  = Rule.Create Vector3.Zero
    }

and [<CasanovaEntity>] Snowflakes = {
  Particles        : RuleTable<Snowflake>    
} with
  static member Particles'(world:World, self:Snowflakes, dt:float32<s>) =
    seq{
      for p in self.Particles do
        if p.Position.Y > 0.0f<m> && Vector3<m>.Distance(p.Position, !world.Camera.Position) < 20.0f<m> then
          yield p
      if self.Particles.Count < 1000 then
        yield Snowflake.Create(world.Camera, world.Sprites)
    } |> Seq.sortBy (fun p -> -Vector3<m>.Distance(p.Position, !world.Camera.Position))


let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
  /// ...
  let sprites_layer = SpriteLayer.Create3D(args.GraphicsDevice, args.Content, BlendState.AlphaBlend)

  /// Setup the observer on (0,0,100)
  let camera = Camera3D.Create(Vector3.UnitZ * 100.0f + Vector3.UnitY * 2.0f, args.GraphicsDevice.Viewport.AspectRatio)

  let snowflakes = 
    {
      Particles = RuleTable.Empty
    }

  let world =
    {
      Sprites     = sprites_layer
      Camera      = camera
      Snowflakes  = snowflakes
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
      wait_key_press Keys.F9      => args.Save("Snowflakes");
      wait_key_press Keys.F10     => args.Load("Snowflakes");

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

