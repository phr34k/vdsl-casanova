module BouncingBalls

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

/// A ball simply contains a physics entity and a drawable sprite.
type [<CasanovaEntity>] Ball = {
  Circle              : PhysicsEntity
  Sprite              : Sprite
} with
  member self.Position = self.Circle.Position
  /// The sprite position and rotation are taken directly from the physics.
  static member SpritePosition'(self:Ball,dt:float32<s>) = 
    self.Circle.Position
  static member SpriteRotation'(self:Ball,dt:float32<s>) = 
    self.Circle.Orientation

/// A wall simply contains a physics entity and a drawable sprite.
/// Ball and Wall are separate entities because extending the sample
/// would likely require adding fields to one or the other. So as a
/// starting point the two entities are almost identical, but in
/// practice it is likely that they would quickly evolve to store
/// different values.
and [<CasanovaEntity>] Wall = {
  Box                 : PhysicsEntity
  Sprite              : Sprite
} with
  /// The sprite position is taken directly from the physics.
  static member SpritePosition'(self:Wall,dt:float32<s>) = 
    self.Box.Position

/// The game world contains:
/// - the physics world (before the physics entities, otherwise
///   updates do not work correctly)
/// - a canvas to draw the entities in a fixed-aspect ratio area of
///   the screen
/// - the list of balls
/// - the list of wall
and [<CasanovaWorld>] World = {
  Physics             : PhysicsWorld
  Canvas              : Canvas
  Balls               : RuleList<Ball>
  Walls               : List<Wall>
} with
  /// Balls are removed when they fall out of the screen.
  static member Balls'(world:World,dt:float32<s>) =
    seq{ for b in !world.Balls do 
           if b.Position.Y < 600.0f<m> then 
             yield b }

let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
  /// We start by initializing the physics world and the canvas.
  let physics = PhysicsWorld.Create()
  /// The canvas is created under the default sprite layer, as we only draw everything there.
  /// The canvas is uniformly stretched, that is it will appear as a square, and it will be 
  /// smaller than the screen.
  let canvas = Canvas.Create(args.DefaultLayer, Vector2.Zero, Vector2.One * 1000.0f, StretchMode.Uniform)

  /// We create a ball with a position (x,y), a radius (r), and a mass of r / 50.
  /// Balls move around, so the physics bodies for the balls are dynamic.
  let mk_ball x y r = 
    {
      Circle    = physics.CreateBall(x, y, r, 1.0f<kg> * r / 50.0f<m>, 0.2f, 0.4f, true)
      Sprite    = Sprite.Create(
                    canvas,
                    Vector2<pixel>.Zero,
                    Vector2<pixel>(r * 2.2f<pixel/m>, r * 2.2f<pixel/m>),
                    @"BouncingBalls\ball")
    }

  /// We create one static body for the ground and one for the left wall.
  /// Walls are still, so the physics bodies for them are static.
  let ground = 
    {
      Box       = physics.CreateBox(0.0f<m>, 495.0f<m>, 500.0f<m>, 10.0f<m>, 0.0f<kg>, 0.0f, 0.4f, false)
      Sprite    = Sprite.Create(
                    canvas,
                    Vector2<pixel>.Zero,
                    Vector2<pixel>(1000.f<pixel>, 10.0f<pixel>),
                    0.0f<rad>,
                    Vector2<pixel>.Zero,
                    @"BouncingBalls\wall", 
                    Color.White, 
                    true)
    }
  let left_wall = 
    {
      Box       = physics.CreateBox(-500.0f<m>, 0.0f<m>, 10.0f<m>, 500.0f<m>, 0.0f<kg>, 0.0f, 0.4f, false)
      Sprite    = Sprite.Create(
                    canvas,
                    Vector2<pixel>.Zero,
                    Vector2<pixel>(10.f<pixel>, 1000.0f<pixel>),
                    0.0f<rad>,
                    Vector2<pixel>.Zero,
                    @"BouncingBalls\wall", 
                    Color.White, 
                    true)
    }

  /// The game world starts with:
  /// - the physics engine
  /// - the canvas
  /// - no balls
  /// - the two walls
  let world =
    {
      Physics = physics
      Canvas  = canvas
      Balls   = RuleList.Empty
      Walls   = [ground; left_wall]
    }

  // Internal requirement. Leave *before* scripts.
  let inline (!) x = immediate_lookup x

  /// We add balls in two different scripts (main and input), so we create an
  /// auxiliary script for this purpose. add_ball creates a ball in a random 
  /// position above the screeen and with a random radius between 25 and 100 m.
  let add_ball =
    co{
      do world.Balls.Add(mk_ball (random_range -400.0f<m> 400.0f<m>) -650.0f<m> (random_range 25.0f<m> 100.0f<m>))
      do! yield_
    }

  /// The main script adds a random ball every second.
  let main = 
    repeat_
     (co{
        do! add_ball
        do! wait 1.0f<s>
      })

  // Five input scripts: 
  // when the user presses the F9 key the game will be saved in BouncingBalls file;
  // when the user presses the F10 key the game will be loaded from BouncingBalls file;
  // when the user presses space a new ball is created
  // when the user presses backspace a ball (if there are any) is removed
  // when the user presses the escape key the pause menu will be pushed on the stack and displayed.
  let input =
    [
      wait_key_press Keys.F9      => args.Save("BouncingBalls");
      wait_key_press Keys.F10     => args.Load("BouncingBalls");
      wait_key_down Keys.Space => 
        co{
          do! add_ball
          do! wait_key_up Keys.Space .||> wait 0.2f<s>
        }
      wait_key_down Keys.Back => 
        co{
          if world.Balls.Count > 0 then
            do world.Balls := world.Balls |> Seq.skip 1
          do! wait_key_up Keys.Back .||> wait 0.2f<s>
        }
      wait_key_press Keys.Escape  => args.PushStack(ConfirmationMenu.start_menu (args.SetStack start_selector), false) 
    ]
  world, main, input
