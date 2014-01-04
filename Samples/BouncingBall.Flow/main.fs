module BouncingBall

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
/// - the ball
/// - the ground
and [<CasanovaWorld>] World = {
  Physics             : PhysicsWorld
  Ball                : Ball
  Ground              : Wall
}

let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
  /// We start by initializing the physics world and the canvas.
  let physics = PhysicsWorld.Create()

  /// We create a ball with a position (x,y), a radius (r), and a mass of r / 50.
  /// Balls move around, so the physics bodies for the balls are dynamic.
  let ball = 
    {
      Circle    = physics.CreateBall(0.0f<m>, 0.0f<m>, 50.0f<m>, 1.0f<kg>, 1.0f, 0.0f, true)
      Sprite    = Sprite.Create(
                    args.DefaultLayer,
                    Vector2<pixel>.Zero,
                    Vector2<pixel>(50.0f * 2.2f<pixel>, 50.0f * 2.2f<pixel>),
                    @"BouncingBalls\ball")
    }

  /// We create a static body for the ground. Walls are still, so the physics bodies for them are static.
  let ground = 
    {
      Box       = physics.CreateBox(0.0f<m>, 495.0f<m>, 500.0f<m>, 10.0f<m>, 0.0f<kg>, 0.0f, 0.0f, false)
      Sprite    = Sprite.Create(
                    args.DefaultLayer,
                    Vector2<pixel>.Zero,
                    Vector2<pixel>(1000.f<pixel>, 10.0f<pixel>),
                    0.0f<rad>,
                    Vector2<pixel>.Zero,
                    @"BouncingBalls\wall", 
                    Color.White, 
                    true)
    }

  /// The game world starts with:
  /// - the physics engine
  /// - the ball
  /// - the ground
  let world =
    {
      Physics = physics
      Ball    = ball
      Ground  = ground
    }

  // Internal requirement. Leave *before* scripts.
  let inline (!) x = immediate_lookup x

  /// The main script does nothing
  let main = yield_

  // Five input scripts: 
  // when the user presses the F9 key the game will be saved in BouncingBalls file;
  // when the user presses the F10 key the game will be loaded from BouncingBalls file;
  // when the user presses the escape key the game will be closed and the main menu will open.
  let input =
    [
      wait_key_press Keys.F9      => args.Save("BouncingBall");
      wait_key_press Keys.F10     => args.Load("BouncingBall");
      wait_key_press Keys.Escape => args.SetStack start_selector
    ]
  world, main, input
