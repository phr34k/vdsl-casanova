module AsteroidShooter

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

// The game world, a representation of our game.
[<CasanovaWorld>]
type World = { 
    // Background draws the background picture
    Background          : SpriteLayer
    // MainSprites draws the ship and the asteroids
    MainSprites         : SpriteLayer
    // ProjectileSprites draws the projectiles, which are translucent
    ProjectileSprites   : SpriteLayer
    // UI draws the fps and score texts
    UI                  : SpriteLayer

    // Asteroids stores all the active asteroids; it is a RuleTable, that is
    // its contents are updated at every frame with the method AsteroidsRule
    Asteroids           : RuleTable<Asteroid>
    // Projectiles stores all the active projectiles; it is a RuleTable, that is
    // its contents are updated at every frame with the method ProjectilesRule
    Projectiles         : RuleTable<Projectile>
    // Cannon contains the user-driven cannon
    Cannon              : Cannon
    
    // DestroyedAsteroids and MissedAsteroids contain the current score of the 
    // game. Note that they are integers, but contained inside a Rule: this 
    // means that they will be updated with the DestroyedAsteroidsRule and 
    // MissedAsteroidsRule methods.
    DestroyedAsteroids  : Rule<int>
    MissedAsteroids     : Rule<int>
    
    // BackgroundSprite contains the sprite that is drawn as the game background
    BackgroundSprite    : Sprite
    
    // ScoreText contains the text that is drawn to represent the game score
    ScoreText           : Text
    
    // FPSSmoothed contains the fps of the game, smoothed
    FPSSmoothed         : Rule<float32>
    
    // FSPText contains the text that is drawn to show the game fps
    FPSText             : Text
  } with
  static member ScoreTextStringRule(world:World,dt:float32<s>) = (string !world.DestroyedAsteroids) + "/" + (string !world.MissedAsteroids)
  static member FPSSmoothedRule(world:World,dt:float32<s>) = lerp (dt * 1.0f<s^-1>) (!world.FPSSmoothed) (1.0f<s> / (max dt 0.001f<s>))
  static member FPSTextStringRule(world:World,dt:float32<s>) = (!world.FPSSmoothed).ToString("0##.0")
  // Increment DestroyedAsteroids by the number of asteroids that have just been destroyed
  static member DestroyedAsteroidsRule(world:World,dt:float32<s>) =
    !world.DestroyedAsteroids +
     (seq{for a in world.Asteroids do
            if !a.Life <= 0.0f then
              yield a} |> Seq.length)
  // return true if the position is visible on the screen
  member world.IsVisible(p:Vector2<m>) = 
    p.X > -600.0f<m> && p.X < 600.0f<m> && p.Y > -600.0f<m> && p.Y < 600.0f<m>
  // Increment MissedAsteroids by the number of asteroids that have just exited the screen
  static member MissedAsteroidsRule(world:World,dt:float32<s>) =
    !world.MissedAsteroids +
     (seq{for a in world.Asteroids do
            if not(world.IsVisible(!a.Position)) then
              yield a} |> Seq.length)
  // Remove all the asteroids that have just exited the screen or that have been 
  // hit by a projectile
  static member AsteroidsRule(world:World,dt:float32<s>) =
    seq{for a in world.Asteroids do
          if !a.Life > 0.0f && world.IsVisible(!a.Position) then
            yield a}
  // Remove all the projectiles that have just exited the screen or that have 
  // hit an asteroid
  static member ProjectilesRule(world:World,dt:float32<s>) =
    seq{for p in world.Projectiles do
          if !p.Life > 0.0f && world.IsVisible(!p.Position) then
            yield p}

// The cannon is aimed by the user to shoot projectiles.
and [<CasanovaEntity>] Cannon = {
  // Position of the cannon on screen
  Position            : Rule<Vector2<m>>
  // Picture used to draw the cannon
  Sprite              : Sprite
  // Current rotational velocity of the cannon
  Movement            : Rule<float32<m/s>>
  } with
  // Movement is slowly reduced to 0
  static member MovementRule(world:World,self:Cannon,dt:float32<s>) = !self.Movement * (1.0f - dt / 1.0f<s>)
  // Angle is incremented by movement
  static member PositionRule(world:World,self:Cannon,dt:float32<s>) = !self.Position + Vector2<1>.UnitX * (!self.Movement * dt)
  // SpritePosition is copied from the angle
  static member SpritePositionRule(world:World,self:Cannon,dt:float32<s>) = !self.Position * 1.0f<pixel/m>

// The asteroids falls down from the top of the screen
and [<CasanovaEntity>] Asteroid = {
    // Position on screen
    Position  : Rule<Vector2<m>>
    // Velocity on screen
    Velocity  : Vector2<m/s>
    // Sprite of this asteroid
    Sprite    : Sprite
    // Residual life of the asteroid
    Life      : Var<float32>
    // Action that destroys projectiles nearby
    DestroyProjectiles : DestroyProjectiles
  } with
  // Convert the position from meters into pixels for the sprite
  static member SpritePositionRule(world:World,self:Asteroid, dt:float32<s>) = !self.Position * 1.0f<pixel/m>
  // Increment the position by the velocity
  static member PositionRule(world:World,self:Asteroid, dt:float32<s>) = !self.Position + dt * self.Velocity

// When an entity contains the DestroyProjectiles action, then it will subtract to the life of all
// asteroids close enough to itself
and [< Action.Action(Target = "Projectile");
       Action.Transfer(Operation = Action.Operation.Set, From = "0.0", To = "Life");
       Action.Radius(25.0f) >] DestroyProjectiles = DestroyProjectiles


// The projectiles move along the direction the cannon was
// aiming when they were shot
and [<CasanovaEntity>] Projectile = {
    // Position on screen
    Position  : Rule<Vector2<m>>
    // Velocity on screen
    Velocity  : Vector2<m/s>
    // Sprite of this projectile
    Sprite    : Sprite
    // Residual life of the projectile
    Life      : Var<float32>
    // Action that destroys asteroids nearby
    DestroyAsteroids : DestroyAsteroids
  } with
  // Convert the position from meters into pixels for the sprite  
  static member SpritePositionRule(world:World,self:Projectile, dt:float32<s>) = !self.Position * 1.0f<pixel/m>
  // Increment the position by the velocity
  static member PositionRule(world:World,self:Projectile, dt:float32<s>) = !self.Position + dt * self.Velocity


// When an entity contains the DestroyAsteroids action, then it will subtract to the life of all
// projectiles close enough to itself
and [< Action.Action(Target = "Asteroid");
       Action.Transfer(Operation = Action.Operation.Set, From = "0.0", To = "Life");
       Action.Radius(25.0f) >] DestroyAsteroids = DestroyAsteroids

// Initializes the game world, main script, and input scripts; takes as input the GPU, 
// the ContentLoader, and the exit function
let rec start_game (start_selector : StartGameSignature<'a>) (args : StartGameArgs) =
// Initialize the world where everything is set to zero
  // all sprite layers are initialized to the default alpha-blending style, 
  // except for the projectiles layer which uses additive blending; note that
  // these lets are scoped within the assignment of the game world
  let background_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  let mainSprites_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  let projectileSprites_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content, BlendState.Additive)
  let ui_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  let world =
    
    {
      Background          = background_layer
      MainSprites         = mainSprites_layer
      ProjectileSprites   = projectileSprites_layer
      UI                  = ui_layer

      Asteroids           = RuleTable.Empty
      Projectiles         = RuleTable.Empty
      DestroyedAsteroids  = Rule.Create(0)
      MissedAsteroids     = Rule.Create(0)
      Cannon              = // We declare a local value to store the position to avoid duplication,
                            // since it is used both for the logical and for the rendering position
                            let cannon_position = Vector2<m>(0.0f<m>, 400.0f<m>)
                            { Position    = Rule.Create(cannon_position)
                              Movement    = Rule.Create(0.0f<m/s>)
                              Sprite      = Sprite.Create(
                                                           mainSprites_layer, cannon_position * Vector2<pixel/m>.One,
                                                           Vector2<pixel>.One * 100.f, @"AsteroidsShooter\ship.png") }
      BackgroundSprite    = Sprite.Create(background_layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 2000.0f, @"AsteroidsShooter\Background1.jpg")
      ScoreText           = Text.Create(ui_layer, @"arial", Vector2<m>(-450.0f<m>, -420.0f<m>) * 1.0f<pixel/m>, -Vector2<pixel>.One * 0.5f, "0", Color.White, Vector2<pixel>.One * 50.f)
      FPSSmoothed         = Rule.Create(60.0f)
      FPSText             = Text.Create(ui_layer, @"arial", Vector2<m>(-450.0f<m>, -470.0f<m>) * 1.0f<pixel/m>, -Vector2<pixel>.One * 0.5f, "?", Color.White, Vector2<pixel>.One * 50.f)
    }

  // For scripts we re-define the lookup operator so that we always interact with the next value 
  // of rules, and not with the current value; this way scripts act as fully imperative threads
  let inline (!) x = immediate_lookup x

  // The main script waits for one to three seconds, creates an asteroid, and then repeats forever
  let main =
    co{
      do! wait (random_range 1.0f<s> 3.0f<s>)
      let x = random_range -500.0f<m> 250.0f<m>
      let y = -550.0f<m>
      world.Asteroids.Add
        { 
          Position  = Rule.Create(Vector2.Create(x,y))
          Velocity  = Vector2<m/s>.UnitY * (random_range 20.0f 80.0f)
          Sprite    = Sprite.Create(
                                     world.MainSprites, 
                                     Vector2<m>(x, y) * 1.0f<pixel/m>, 
                                     Vector2<pixel>.One * 50.f, 
                                     @"AsteroidsShooter\asteroid1.png")
          Life      = var 1.0f
          DestroyProjectiles = DestroyProjectiles
        }
    } |> repeat_

  // The input scripts are a list of event-detection scripts that wait for an event to happen,
  // followed by event response scripts that perform some action in response to an event
  let input =
    [
      // Wait for the space key to be pressed...
      wait_key_down Keys.Space =>
      // ... create two projectiles (one for each wing)
      co{
        // We define a local function to add a projectile with a certain horizontal displacement
        let add_projectile x_delta =
          let delta = Vector2<m>.UnitX * x_delta
          let p = !world.Cannon.Position + delta
          world.Projectiles.Add
            { 
              Position = Rule.Create(p)
              Velocity = -Vector2<m/s>.UnitY * 500.0f
              Sprite    = Sprite.Create(
                                         world.ProjectileSprites, p * 1.0f<pixel/m>, 
                                         Vector2<pixel>(5.f<pixel>,20.f<pixel>), 
                                         0.0f<_>, Vector2<pixel>.Zero,
                                         @"AsteroidsShooter\plasma_small.jpg", 
                                         Color.White, true)
              Life      = var 1.0f
              DestroyAsteroids = DestroyAsteroids
            }
        // add right wing projectile
        do add_projectile 5.0f
        // add left wing projectile
        do add_projectile -5.0f
        // wait until either the space key is released or 0.2 seconds pass, to avoid spawning infinite projectiles
        do! (wait_key_up Keys.Space) .||> (wait 0.2f<s>) }
      
      // When left is pressed, set cannon rotation velocity to -2rad/s
      wait_key_down Keys.Left => co{ return world.Cannon.Movement := !world.Cannon.Movement - 20.0f<m/s> }

      // When right is pressed, set cannon rotation velocity to +2rad/s
      wait_key_down Keys.Right => co{ return world.Cannon.Movement := !world.Cannon.Movement + 20.0f<m/s> }
      
      // When escape is pressed, push the menu to the stack
      wait_key_press Keys.Escape => args.PushStack (start_pause_menu start_selector, false)
    ]
  // return the world, main script, and input scripts as the result of the start_game function
  world, main, input

// Used to allow menu to run a new game
and start_main_menu(start_selector : StartGameSignature<'a>) =
  MainMenu.start_menu  "AsteroidShooter" @"AsteroidsShooter\Background1.jpg" start_game start_selector//(start_game start_selector : StartGameSignature<World>)

and start_pause_menu(start_selector : StartGameSignature<'a>) =
  PauseMenu.start_menu "AsteroidShooter" @"AsteroidsShooter\Background1.jpg" start_game start_selector//(start_game start_selector : StartGameSignature<World>)

