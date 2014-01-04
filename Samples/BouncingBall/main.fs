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
type [<CasanovaEntity>] Ball = {
    Velocity              : Rule<Vector3<m/s>>
    Sprite3D              : Sprite3D
} with
    member self.Position3D = !self.Sprite3D.Position
    static member Sprite3DPosition'(world:World,self:Ball,dt:float32<s>) = 
        !self.Sprite3D.Position - Vector3<m/s>.UnitY * dt * 0.1f + !self.Velocity * dt * 0.1f
    static member Velocity'(world:World,self:Ball,dt:float32<s>) = 
        !self.Velocity + Vector3<m/s>.Create(random_range -0.1f 0.1f, random_range -0.1f 0.1f, random_range -0.1f 0.1f)
    static member Sprite3DRotation'(world:World,self:Ball,dt:float32<s>) = 
        Quaternion.CreateFromRotationMatrix(Matrix.CreateConstrainedBillboard(self.Position3D.ToXNA, (!world.Camera.Position).ToXNA, Vector3.UnitY.ToXNA, System.Nullable(world.Camera.Forward.ToXNA), System.Nullable()))
    static member Sprite3DColor'(world:World,self:Ball,dt:float32<s>) = 
        Color.White * (lerp (Vector3<m>.Distance(self.Position3D, !world.Camera.Position) / 200.0f<m>) 0.0f 1.0f)
    static member Create(camera:Camera3D, sprite_layer:SpriteLayer) =
        let snow_field = 20.0f
        let offset = Vector3<m>.Create(random_range -snow_field snow_field, random_range (-snow_field * 0.5f) (snow_field * 0.5f), random_range -snow_field snow_field)
        {
            Sprite3D    = Sprite3D.Create(sprite_layer, !camera.Position + offset, Quaternion.Identity, Vector2<m>.One * 0.1f, Color.White, @"Particles\snowflake")
            Velocity  = Rule.Create Vector3.Zero
        }

and [<CasanovaWorld>] World = {
    Sprites              : SpriteLayer
    Camera              : Camera3D
    Balls              : RuleList<Ball>    
} with
    static member Balls'(world:World,self:World,dt:float32<s>) = 
        let s = self.Balls
        let s = seq{ for p in s do 
                        if p.Position3D.Y > 0.0f<m> && Vector3<m>.Distance(p.Position3D, !world.Camera.Position) < 20.0f<m> then  
                           yield p 
                     if s.Count < 1000 then
                        yield Ball.Create(world.Camera, world.Sprites)                           
                   } |> Seq.sortBy (fun (p : Ball) -> -Vector3<m>.Distance(p.Position3D, !world.Camera.Position))
        s
    static member SpritesView'(world:World,self:World,dt:float32<s>) = 
        self.Camera.View
    static member SpritesProjection'(world:World,self:World,dt:float32<s>) = 
        self.Camera.Projection


let rec start_game (args : StartGameArgs) =
    let sprites_layer =
        SpriteLayer.Create3D(args.GraphicsDevice, args.Content, BlendState.AlphaBlend)
    let camera =
        Camera3D.Create(Vector3.UnitZ * 100.0f + Vector3.UnitY * 2.0f, args.GraphicsDevice.Viewport.AspectRatio)
    /// The game world starts with:
    /// - the physics engine
    /// - the ball
    /// - the ground
    let world =
      {        
        Sprites    = sprites_layer
        Camera    = camera
        Balls    = RuleList.Empty
      }
    let snow_field =
        20.0f
    // Internal requirement. Leave *before* scripts.
    let inline (!) x = immediate_lookup x
    /// The T12F73E33 script is generated
    let T12F73E33 = 
      [
          wait_key_press Keys.F9      => args.Save("Snowflakes");
          wait_key_press Keys.F10     => args.Load("Snowflakes");

          wait_mouse_move =>> 
            fun (mx,my) ->
              co{
                do world.Camera.Rotation := Vector2.Create(mx,my) * 0.1f<rad/s>
                //do set_mouse_position(200,200)
              };

          wait_key_down  Keys.W       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Forward * 30.0f<m/s> }
          wait_key_down  Keys.A       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Left * 30.0f<m/s> }
          wait_key_down  Keys.S       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Backward * 30.0f<m/s> }
          wait_key_down  Keys.D       => co{ return world.Camera.Velocity := !world.Camera.Velocity + world.Camera.Right * 30.0f<m/s> }
          wait_key_press Keys.Escape  => 
            co{
              do show_mouse_cursor()              
            }        
      ]
    /// Generated: The main script does nothing
    let main = yield_
    /// The input script does nothing
    let input = []
    let input = List.append input T12F73E33
    /// The main script does nothing
    world, main, input

[<CasanovaEntryPoint>]
let Run() =
    let game = Game.Create(start_game, 1024, 768, false, "Bouncing Ball") 
    game.Run()
