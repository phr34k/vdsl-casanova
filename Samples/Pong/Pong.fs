module Pong

open Casanova
open Casanova.Core
open Casanova.Coroutines
open Casanova.Utilities
open Casanova.Input
open Casanova.Math
open Casanova.Game
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.UI.Controls
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

/// A wall simply contains a physics entity and a drawable sprite.
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
/// - a ball
/// - the list of wall
/// - the player paddles
/// - the player scores and texts to draw them
and [<CasanovaWorld>] World = {
  Physics             : PhysicsWorld
  Canvas              : Canvas
  Ball                : Ball
  Walls               : List<Wall>
  Paddle1             : Wall
  Paddle2             : Wall
  Player1Score        : Var<int>
  Player2Score        : Var<int>
  Player1ScoreText    : Text
  Player2ScoreText    : Text
} with 
  static member Player1ScoreTextString'(world:World, dt:float32<s>) = "Score:" + (string !world.Player1Score)
  static member Player2ScoreTextString'(world:World, dt:float32<s>) = "Score:" + (string !world.Player2Score)

// Initializes the game world, main script, and input scripts.
let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
    // We put the whole game into a canvas. This way even if the screen changes resolution, the game
    // elements will not be stretched. The side effect is that the playing field will not be the whole
    // screen, but rather a (square) portion of it.
    let canvas = Canvas.Create(args.DefaultLayer, Vector2<pixel>.Zero, Vector2<pixel>.One * 890.f, StretchMode.Uniform)

    let physics = PhysicsWorld.Create(Vector2.Zero)

    /// We create a ball with a position (x,y), a radius (r), and a mass of r / 50.
    /// Balls move around, so the physics bodies for the balls are dynamic.
    let ball = 
      {
        Circle    = physics.CreateBall(0.0f<m>, 0.0f<m>, 10.0f<m>, 1.0f<kg>, 1.0f, 0.1f, true)
        Sprite    = Sprite.Create(
                      canvas,
                      Vector2<pixel>.Zero,
                      Vector2<pixel>(20.0f<pixel>, 20.0f<pixel>),
                      @"Pong\ball")
      }

    /// We create a static body for each wall. Walls are still, so the physics bodies for them are static.
    let ground = 
      {
        Box       = physics.CreateBox(0.0f<m>, 495.0f<m>, 500.0f<m>, 10.0f<m>, 0.0f<kg>, 0.0f, 0.0f, false)
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
    let ceiling = 
      {
        Box       = physics.CreateBox(0.0f<m>, -495.0f<m>, 500.0f<m>, 10.0f<m>, 0.0f<kg>, 0.0f, 0.0f, false)
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
    let left = 
      {
        Box       = physics.CreateBox(-495.0f<m>, 0.0f<m>, 10.0f<m>, 500.0f<m>, 0.0f<kg>, 0.0f, 0.0f, false)
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
    let right = 
      {
        Box       = physics.CreateBox(495.0f<m>, 0.0f<m>, 10.0f<m>, 500.0f<m>, 0.0f<kg>, 0.0f, 0.0f, false)
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

    let paddles_y = 400.0f<m>

    let paddle1 = 
      {
        Box       = physics.CreateBox(0.0f<m>, -paddles_y, 100.0f<m>, 10.0f<m>, 1.0f<kg>, 0.0f, 0.2f, true)
        Sprite    = Sprite.Create(
                      canvas,
                      Vector2<pixel>.Zero,
                      Vector2<pixel>(200.f<pixel>, 20.0f<pixel>),
                      0.0f<rad>,
                      Vector2<pixel>.Zero,
                      @"Pong\paddle1", 
                      Color.White, 
                      true)
      }
    do physics.AddFixedPrismaticJoint(paddle1.Box, Vector2.UnitX)

    let paddle2 = 
      {
        Box       = physics.CreateBox(0.0f<m>, paddles_y, 100.0f<m>, 10.0f<m>, 1.0f<kg>, 0.0f, 0.2f, true)
        Sprite    = Sprite.Create(
                      canvas,
                      Vector2<pixel>.Zero,
                      Vector2<pixel>(200.f<pixel>, 20.0f<pixel>),
                      0.0f<rad>,
                      Vector2<pixel>.Zero,
                      @"Pong\paddle2", 
                      Color.White, 
                      true)
      }
    do physics.AddFixedPrismaticJoint(paddle2.Box, Vector2.UnitX)

    let world =
      { 
        Physics   = physics
        Canvas    = canvas
        Ball      = ball
        Walls     = [ ground; ceiling; left; right ]
        Paddle1   = paddle1
        Paddle2   = paddle2
        Player1Score     = var 0
        Player2Score     = var 0
        Player1ScoreText = Text.Create(canvas, "Segoe", Vector2<pixel>.Create(0.0f, 530.0f), 
                                               Vector2<pixel>.Zero, "", Color.PaleGreen, Vector2<pixel>.One * 100.f)        
        Player2ScoreText = Text.Create(canvas, "Segoe", Vector2<pixel>.Create(0.0f, -530.0f),
                                               Vector2<pixel>.Zero, "", Color.Aqua, Vector2<pixel>.One * 100.f)
      }

    // For scripts we re-define the lookup operator so that we always interact with the next value 
    // of rules, and not with the current value; this way scripts act as fully imperative threads
    let inline (!) x = immediate_lookup x

    let reset_paddles_and_ball = 
      co{
        do ball.Circle.Position <- Vector2<m>.Zero
        do paddle1.Box.Position <- Vector2.UnitY * paddles_y
        do paddle2.Box.Position <- -Vector2.UnitY * paddles_y
        do ball.Circle.ResetDynamics()
        do paddle1.Box.ResetDynamics()
        do paddle2.Box.ResetDynamics()
        
        do! wait 1.0f<s>
        do ball.Circle.ApplyLinearImpulse((Vector2<m/s>.Create(random_range -0.5f 0.5f, if random_int() % 2 = 0 then 1.0f else -1.0f) * 30.0f))
      }

    // the main script launches the ball
    let main = 
      co{
        do! reset_paddles_and_ball
      }


    let input =
      [
            // When left key is pressed, move the first paddle to the left
            wait_key_down Keys.Left => co { return world.Paddle1.Box.ApplyForce(-Vector2<N>.UnitX * 400.0f) }
            // When right key is pressed, move the first paddle to the right
            wait_key_down Keys.Right => co { return world.Paddle1.Box.ApplyForce(Vector2<N>.UnitX * 400.0f) }
            // When A key is pressed, move the second paddle to the left
            wait_key_down Keys.A => co { return world.Paddle2.Box.ApplyForce(-Vector2<N>.UnitX * 400.0f) }
            // When D key is pressed, move the second paddle to the right
            wait_key_down Keys.D => co { return world.Paddle2.Box.ApplyForce(Vector2<N>.UnitX * 400.0f) }
            // When ESC key is pressed, quit the game
            wait_key_press Keys.Escape  => args.PushStack(ConfirmationMenu.start_menu (args.SetStack start_selector), false) 
            wait_condition (fun () -> world.Ball.Position.Y < -450.0f<m>) => 
              co{ 
                do world.Player1Score := !world.Player1Score + 1
                do! reset_paddles_and_ball
              }
            wait_condition (fun () -> world.Ball.Position.Y > 450.0f<m>) => 
              co{ 
                do world.Player2Score := !world.Player2Score + 1
                do! reset_paddles_and_ball
              }
      ]
    // return the world, main script, and input scripts as the result of the start_game function
    world, main, input
