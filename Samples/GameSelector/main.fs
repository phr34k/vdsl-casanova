module GameSelector

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Casanova
open Casanova.Core
open Casanova.Coroutines
open Casanova.Utilities
open Casanova.Input
open Casanova.Math
open Casanova.Game
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.Core
open Casanova.StandardLibrary.UI.Controls


[<CasanovaWorld>]
type World = 
  {
    GamesLayer        : SpriteLayer
    Background        : Sprite
    HeaderCanvas      : Canvas
    BodyCanvas        : Canvas
    HeaderText        : Text
    Body              : Body
    Footer            : Footer
  }

and [<CasanovaEntity>] Body = 
  {
    ListCanvas            : Canvas
    DescriptionCanvas     : Canvas
    GameList              : ListBox<GameDescription>
    DescriptionFrame      : Border
    ImageFrame            : Border
    Image                 : Rule<Sprite>
    Description           : Rule<WrappingText>
  }
  static member ImageRule(world:World,self:Body,dt:float32<s>) = 
      Sprite.Create(self.DescriptionCanvas, self.Image.Value.Position.Value, self.Image.Value.Size.Value, self.GameList.SelectedItem.ImagePath)
  static member DescriptionRule(world:World,self:Body,dt:float32<s>) = 
      WrappingText.Create(self.DescriptionCanvas, "Arial", Vector2<_>.Create(-480.0f, 20.0f), self.GameList.SelectedItem.Description, Color.White, Vector2<_>.Create(960.0f, 460.f))
  static member Create(canvas : Canvas, games : GameDescription[]) = 
      let list_canvas         = Canvas.Create(canvas, Vector2<_>.UnitX * -250.f<_>, Vector2<_>(500.f<_>, 1000.f<_>), Vector2<1>.One, StretchMode.Stretch)
      let description_canvas  = Canvas.Create(canvas, Vector2<_>.UnitX * 250.f<_>, Vector2<_>(500.f<_>, 1000.f<_>), Vector2<1>.One, StretchMode.Stretch)
      let image_position      = Vector2<pixel>.UnitY * -250.f<1>
      let description_position= Vector2<pixel>.UnitY * 250.f<1>
      let image_size = Vector2<pixel>(850.f<_>, 400.f<_>)
      {
        ListCanvas            = list_canvas
        DescriptionCanvas     = description_canvas
        GameList              = ListBox.Create(list_canvas, games, Vector2<_>.Zero, Vector2<pixel>.One * 1000.f, 1000.f<_>, 60.f<_>, 5.f<pixel>, Color.White)
        DescriptionFrame      = Border.Create(description_canvas, Rectangle.FromCenterSize(description_position, Vector2<_>(1000.f<_>, 500.f<_>)), 5.f<pixel>)
        ImageFrame            = Border.Create(description_canvas, Rectangle.FromCenterSize(image_position, Vector2<_>(1000.f<_>, 500.f<_>)), 5.f<pixel>)
        Image                 = Rule.Create(fun () -> Sprite.Create(description_canvas, image_position, image_size, games.[0].ImagePath))
        Description           = Rule.Create(fun () -> WrappingText.Create(description_canvas, "Arial", Vector2<_>.Zero, "", Color.White, Vector2<_>.One * 1000.f))
      }
    
and [<CasanovaEntity>] Footer =
  {
    Canvas              : Canvas
    ConfirmButton       : Button
    QuitButton          : Button    
  }

and [<ReferenceEquality>] GameDescription = 
  {
    Name          : string
    Description   : string 
    ImagePath     : string
    Launcher      : Coroutine<unit>        
  }
  override this.ToString() = this.Name
  static member Create (name, description, imagePath, launcher) =
    {
      Name        = name
      Description = description
      ImagePath   = imagePath
      Launcher    = launcher
    }

let rec start_selector (args : StartGameArgs) =
  // The element layer
  let element_layer = SpriteLayer.Create(args.GraphicsDevice, args.Content)
  
  let asteroidShooter = GameDescription.Create(
                          "Asteroid Shooter",
                          "This sample is a small playable game. Control the ship with the LEFT and RIGHT arrows and shoot with SPACEBAR at the incoming projectiles.",
                          @"GamesImages\AsteroidShooter",
                          args.SetStack asteroid_launcher)
  (*                            
  let bouncingBall = GameDescription.Create(
                          "Bouncing ball",
                          "In this sample we see a single ball with some physics. F9 saves and F10 loads.",
                          @"GamesImages\BouncingBall",
                          args.SetStack bouncing_ball_launcher)
  *)                        

  let bouncingBalls = GameDescription.Create(
                          "Bouncing balls",
                          "In this sample we see multiple balls with some phsyics. When a ball exits the screen it is also removed from the game world. F9 saves and F10 loads.",
                          @"GamesImages\BouncingBalls",
                          args.SetStack bouncing_balls_launcher)

  let cellularAutom = GameDescription.Create(
                          "Fireflies",
                          "This sample is a small simulator where fireflies move randomly across a field.",
                          @"GamesImages\Fireflies",
                          args.SetStack cellular_autom_launcher)

  let pong = GameDescription.Create(
                          "Pong",
                          "The classic game of virtual tennis Pong. Left and Right move payer 1, A and D move player2.",
                          @"GamesImages\Pong",
                          args.SetStack pong_launcher)
  let pac_man = GameDescription.Create(
                          "PacMan",
                          "The classic game of Pac Man. Run away from the ghosts and win the level by eating all the yellow balls.",
                          @"PacMan\screen.png",
                          args.SetStack pac_man_launcher)

  let rts = GameDescription.Create(
                          "RTS",
                          "This sample is a small, playable RTS game. Wait for your turn, and then select (drag with the mouse) ships from your planets and send them (right click) to destroy the enemy. The camera is controlled with WASD.",
                          @"GamesImages\RTS",
                          args.SetStack rts_launcher)

  let tetris = GameDescription.Create(
                          "Tetris",
                          "This sample is a Tetris-like game. Delete more lines at the same time in order to get higher scores. Rotate with Up, move with directional keys.",
                          @"GamesImages\Tetris",
                          args.SetStack tetris_launcher)

  let rotatingCube = GameDescription.Create(
                          "Rotating Cube",
                          "This sample is a 3D, rotating cube. WASD and the mouse move the camera, arrows rotate the cube.",
                          @"GamesImages\RotatingCube",
                          args.SetStack rotating_cube_launcher)

  let fallingCubes = GameDescription.Create(
                          "Falling Cubes",
                          "This sample is a bunch of 3D cubes with physics. WASD and the mouse move the camera, space spawns new cubes.",
                          @"GamesImages\FallingCubes",
                          args.SetStack falling_cubes_launcher)

  let snowflakes   = GameDescription.Create(
                          "Snow particles",
                          "This sample is a particle system simulating snowflakes. WASD and the mouse move the camera.",
                          @"GamesImages\Snowflakes",
                          args.SetStack snowflakes_launcher)

  let world =
    let body_canvas = Canvas.Create(element_layer, Vector2<_>.Zero, Vector2<_>(1000.f<_>, 800.f<_>), StretchMode.Uniform)
    let games =
        [| 
          asteroidShooter
          (*bouncingBall*)
          bouncingBalls
          cellularAutom
          pong
          rts
          tetris
          rotatingCube
          fallingCubes
          snowflakes
          pac_man
        |]
    let games = Body.Create(body_canvas, games)

    let footer_canvas = Canvas.Create(element_layer, Vector2<_>.UnitY * 450.f<_>, Vector2<_>(1000.f<_>, 100.f<_>), StretchMode.Uniform)
    let footer = 
      {
        Canvas          = footer_canvas
        QuitButton      = Button.Create(footer_canvas, Vector2<_>.UnitX * -250.f<_>, Vector2<_>(400.f<_>, 800.f<_>), "Quit", Color.White, 50.f<pixel>)
        ConfirmButton   = Button.Create(footer_canvas, Vector2<_>.UnitX * 250.f<_>, Vector2<_>(400.f<_>, 800.f<_>), "Launch", Color.White, 50.f<pixel>)
      }
    let header_canvas = Canvas.Create(element_layer, Vector2<_>.UnitY * -450.f<_>, Vector2<_>(1000.f<_>, 100.f<_>), StretchMode.Stretch)
    {
      Background        = Sprite.Create(element_layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 1000.f, 0.f<rad>, Vector2<pixel>.Zero, @"UI/white_pixel", UI.Theme.ElementBackground, true)
      BodyCanvas        = body_canvas
      HeaderCanvas      = header_canvas
      HeaderText        = Text.Create(header_canvas, "Segoe", Vector2<_>.Zero, Vector2<_>.Zero, "Casanova samples", Color.White, Vector2<_>.One * 1000.f<_>)
      GamesLayer        = element_layer
      Body              = games
      Footer            = footer
    }
    
  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x

  // the main script does nothing
  let main = yield_

  do Coroutines.run_script (wait_condition (fun () -> !world.Footer.QuitButton.IsSelected) => args.Quit)
  do Coroutines.run_script (wait_condition (fun () -> !world.Footer.ConfirmButton.IsSelected) => co{ return! world.Body.GameList.SelectedItem.Launcher }) 

  // Input scripts are three: 
  // when the user presses the escape key we invoke the exit method that closes the application
  // when the user presses the F9 key we save the game in BouncingBall
  // when the user presses the F10 key we load the game from BouncingBall
  let input =
    [
      wait_key_down Keys.Escape => args.Quit
      wait_key_press Keys.Enter => co{ return! world.Body.GameList.SelectedItem.Launcher }
      wait_key_press Keys.Up    => 
        co{ 
          do world.Body.GameList.SelectPrevius() 
          }
      wait_key_press Keys.Down  => co{ do world.Body.GameList.SelectNext() }
    ]
  world, main, input

and asteroid_launcher       = AsteroidShooter.start_main_menu start_selector
//and bouncing_ball_launcher  = BouncingBall.start_game start_selector
and bouncing_balls_launcher = BouncingBalls.start_game start_selector
and cellular_autom_launcher = CellularAutomaton.start_game start_selector
and pong_launcher           = Pong.start_game start_selector
and rts_launcher            = RTS.Main.start_main_menu start_selector
and tetris_launcher         = Tetris.start_game start_selector
and rotating_cube_launcher  = RotatingCube.start_game start_selector
and falling_cubes_launcher  = FallingCubes.start_game start_selector
and snowflakes_launcher     = SnowParticles.start_game start_selector
and pac_man_launcher        = PacMan.start_game start_selector

[<CasanovaEntryPoint>]
let Run() =
  // We start the game in fullscreen and with a resolution of 1024x768 pixels
  let game = Game.Create(start_selector, 1024, 768, true, "CasanovaSamplesGUI") 
  game.Run()
