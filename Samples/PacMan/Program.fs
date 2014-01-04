module PacMan

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
open GameDefinition
open MapParser
open MenuGameOver


(**************************************************START GAME*************************************************)
let rec start_game (start_selector : StartGameSignature<'a>)(args : StartGameArgs) =
   let physics = PhysicsWorld.Create (Vector2<m/s^2>.Zero)
   let canvas = Canvas.Create(args.DefaultLayer, Vector2<pixel>.Zero, Vector2<pixel>.One * 850.0f, StretchMode.Uniform)
   let walls = 
    [for (y,x,w) in h_walls do
      let pos_x = ((float32(x) - 13.5f) / 13.5f) * 500.0f<m>
      let pos_y = ((float32(y) - 15.5f) / 15.5f) * 500.0f<m>
      let length = ((float32(w)-1.0f) / 27.0f) * 1000.0f<m>
      let pos_x = (pos_x + length/2.0f)
      let pos_y = (pos_y + 2.0f<m>)
      yield
        {
          Box = physics.CreateBox (pos_x, pos_y, (length - 13.5f<m>) * 0.5f, 2.0f<m>,0.0f<kg>,0.0f,0.0f,false)
          Sprite = Sprite.Create (canvas,Vector2.Create (pos_x, pos_y),Vector2<pixel>.Create(length * 1.0f<_>,4.0f<pixel>),0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\wall", Color.Red,true)
        }
      for (x,y,h) in v_walls do
        let pos_x = ((float32(x) - 13.5f) / 13.5f) * 500.0f<m>
        let pos_y = ((float32(y) - 15.5f) / 15.5f) * 500.0f<m>
        let height = ((float32(h)-1.0f) / 31.0f) * 1000.0f<m>
        let pos_x = (pos_x + 2.0f<m>)
        let pos_y = (pos_y + height/2.0f)
        yield
          {
            Box = physics.CreateBox (pos_x, pos_y, 2.0f<m>, (height-15.5f<m>)*0.5f, 0.0f<kg>,0.0f,0.0f,false)
            Sprite = Sprite.Create (canvas,Vector2.Create (pos_x, pos_y),Vector2<pixel>.Create(4.0f<pixel>,height*1.0f<_>),0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\wall", Color.Red,true)
          }
    ]
   let balls =
    seq{
          for (y,x) in dots do
            let pos_x = ((float32(x) - 13.5f) / 13.5f) * 500.0f<m>
            let pos_y = ((float32(y) - 15.5f) / 15.5f) * 500.0f<m>
            yield{
                    Ball.Position = Vector2<m>.Create(pos_x,pos_y)
                    Ball.Sprite = Sprite.Create (canvas, Vector2.Create(pos_x,pos_y), Vector2<pixel>.Create(90.0f,50.0f), 0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\pixel.png", Color.Yellow,true)
                 }
       } 
    
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
   let pacman:PacMan = {
    Circle = physics.CreateBall (100.0f<m>,100.0f<m>,20.0f<m>,0.0f<kg>,0.0f,0.0f,true)
    Sprite = Sprite.Create (canvas, Vector2.Create(100.0f<m>,100.0f<m>), Vector2<pixel>(40.0f<pixel>,40.0f<pixel>), @"PacMan\pacman.png")
    Life = Rule.Create(4) 
    Score = Rule.Create(-10)
   }
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    let fruit:Fruit = {
      Fruit.Position = Vector2.Create (55.5555573f<m>,-16.1290321f<m>)
      Fruit.Sprite = Sprite.Create (canvas, Vector2.Create(55.5555573f<m>,-16.1290321f<m>), Vector2<pixel>(50.0f<pixel>,50.0f<pixel>), @"PacMan\GhostOrange.png") 
    }
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   let superballs:seq<SuperBall> = 
    seq{  
          let pos1_x = ((float32(1) - 13.5f) / 13.5f) * 500.0f<m>
          let pos1_y = ((float32(1) - 15.5f) / 15.5f) * 500.0f<m>
          let pos2_x = ((float32(25) - 13.5f) / 13.5f) * 500.0f<m>
          let pos2_y = ((float32(25) - 15.5f) / 15.5f) * 500.0f<m>
          yield{
                  SuperBall.Position = Vector2<m>.Create(pos1_x,pos1_y)
                  SuperBall.Sprite = Sprite.Create (canvas, Vector2.Create(pos1_x,pos1_y), Vector2<pixel>.Create(150.0f,100.0f), 0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\pixel.png", Color.Yellow,true)
                }
          
          yield{
                  SuperBall.Position = Vector2<m>.Create(pos2_x,pos1_y)
                  SuperBall.Sprite = Sprite.Create (canvas, Vector2.Create(pos2_x,pos1_y), Vector2<pixel>.Create(150.0f,100.0f), 0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\pixel.png", Color.Yellow,true)
                }
          yield{
                  SuperBall.Position = Vector2<m>.Create(pos1_x,pos2_y)
                  SuperBall.Sprite = Sprite.Create (canvas, Vector2.Create(pos1_x,pos2_y), Vector2<pixel>.Create(150.0f,100.0f), 0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\pixel.png", Color.Yellow,true)
                }
          yield{
                  SuperBall.Position = Vector2<m>.Create(pos2_x,pos2_y)
                  SuperBall.Sprite = Sprite.Create (canvas, Vector2.Create(pos2_x,pos2_y), Vector2<pixel>.Create(150.0f,100.0f), 0.0f<rad>, Vector2<pixel>.Zero, @"PacMan\pixel.png", Color.Yellow,true)
                }
       }  

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   
   let ghost () = 
    seq{
      yield{
            Position = Rule.Create(Vector2.Create (55.5555573f<m>,-16.1290321f<m>))
            Velocity = Var.Create (Vector2.UnitX)
            Sprite = Sprite.Create (canvas, Vector2.Create(55.5555573f<m>,-16.1290321f<m>), Vector2<pixel>(50.0f<pixel>,50.0f<pixel>), @"PacMan\GhostBlue.png") 
           }
      yield{
            Position = Rule.Create(Vector2.Create (55.5555573f<m>,-16.1290321f<m>))
            Velocity = Var.Create (Vector2.UnitY)
            Sprite = Sprite.Create (canvas, Vector2.Create(55.5555573f<m>,-16.1290321f<m>), Vector2<pixel>(50.0f<pixel>,50.0f<pixel>), @"PacMan\GhostPink.png") 
           }
      yield{
            Position = Rule.Create(Vector2.Create (55.5555573f<m>,-16.1290321f<m>))
            Velocity = Var.Create (Vector2.UnitY)
            Sprite = Sprite.Create (canvas, Vector2.Create(55.5555573f<m>,-16.1290321f<m>), Vector2<pixel>(50.0f<pixel>,50.0f<pixel>), @"PacMan\GhostOrange.png") 
           }
      yield{
            Position = Rule.Create(Vector2.Create (55.5555573f<m>,-16.1290321f<m>))
            Velocity = Var.Create (Vector2.UnitY)
            Sprite = Sprite.Create (canvas, Vector2.Create(55.5555573f<m>,-16.1290321f<m>), Vector2<pixel>(50.0f<pixel>,50.0f<pixel>), @"PacMan\GhostGreen.png") 
           }
      }
   
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   
   let world = {
    Physics = physics
    Canvas = canvas
    Balls = RuleList.Create (fun () -> balls)
    Walls = walls
    PacMan = pacman
    Ghosts = RuleList.Create (fun () -> ghost())
    Hearts = RuleList.Create (fun () -> Seq.empty)
    ScoreText = Text.Create (canvas, Vector2.Create(-600.0f, -450.0f), Vector2<pixel>.Create(200.0f, 100.0f))
    SuperBalls = RuleList.Create (fun () -> superballs)
    Fruits = RuleList.Empty
   }

   let fruit =
    repeat_
      (co{
        do! wait 5.0f<s>
        world.Fruits.Add (fruit)
        }
       )
   let movement = 
     repeat_
      (co{
        do! wait 0.3f<s>
        do for g in world.Ghosts do
              let vs= [
                      for (y,x,l,r,u,d) in crossings do
                        let x = ((float32(x) - 13.5f) / 13.5f) * 500.0f<m>
                        let y = ((float32(y) - 15.5f) / 15.5f) * 500.0f<m>              
                        if Vector2.Distance (Vector2.Create (x*1.0f<m>,y*1.0f<m>), !g.Position) < 25.0f<m>  
                          then
                            if u then yield -Vector2.UnitY, Vector2<m>.Create(x,y)
                            if d then yield Vector2.UnitY, Vector2<m>.Create(x,y)
                            if l then yield -Vector2.UnitX, Vector2<m>.Create(x,y)
                            if r then yield Vector2.UnitX, Vector2<m>.Create(x,y)
                      ]
              let vi= Vector2.Normalize(!g.Position - world.PacMan.position)
              let v1=
                [
                for i,v in List.mapi (fun i v -> i,v) vs  do
                  yield i, Vector2.Dot (fst v,vi), snd v  
                ]
              if v1 <> [] then
                let i_min,_,p' = List.minBy (fun (i,v,p) -> v) v1
                let v1 = 
                  if random_float() >= 0.2f then fst vs.[i_min]
                  else 
                    let length = vs.Length - 1
                    fst vs.[random_interval 0 length]
                g.Velocity := v1 * 100.0f<m/s>
//                let p'' = v1 * 5.0f<m>
//                let p''' = p' + p''
                g.Position := p'
                       
          })
   let die = 
      repeat_
        (co{
            do! yield_
            let dead = Seq.exists ( fun g -> Vector2.Distance (!g.Position,world.PacMan.position) < 45.0f<m>) world.Ghosts
            if dead then

              if !world.PacMan.Life=0 then 
                  do! args.PushStack(start_game_over_menu start_selector, false);
                  
              else
                  let new_ghosts = ghost()
                  world.Ghosts.Clear ()
                  for g in new_ghosts do
                    world.Ghosts.Add (g)
                  world.PacMan.Circle.ResetDynamics ()
                  world.PacMan.Circle.Position <- Vector2.Create(100.0f<m>,100.0f<m>)
              
         })
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 
   let main = 
    co{
        do! movement .&& die |> ignore_
      }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   let inline (!) x = immediate_lookup x

   let input = 
     [//args.SetStack start_selector)
            wait_key_press Keys.Escape => args.PushStack(start_pause_menu start_selector, false);
            wait_key_press Keys.Up => co {
                world.PacMan.Circle.ResetDynamics ()
                world.PacMan.Circle.ApplyLinearImpulse (-Vector2<m/s>.UnitY*pacman_speed) };
            wait_key_press Keys.Down => co {
                world.PacMan.Circle.ResetDynamics ()
                world.PacMan.Circle.ApplyLinearImpulse (Vector2<m/s>.UnitY*pacman_speed) };
            wait_key_press Keys.Left => co {
                world.PacMan.Circle.ResetDynamics ()
                world.PacMan.Circle.ApplyLinearImpulse (-Vector2<m/s>.UnitX*pacman_speed) };
            wait_key_press Keys.Right => co {
                world.PacMan.Circle.ResetDynamics ()
                world.PacMan.Circle.ApplyLinearImpulse (Vector2<m/s>.UnitX*pacman_speed) };
           
     ]
   world,main,input
   
and start_main_menu(start_selector : StartGameSignature<'a>) =
  MainMenu.start_menu  "Pac Man" @"PacMan\screen.png" start_game start_selector//(start_game start_selector : StartGameSignature<World>)

and start_pause_menu(start_selector : StartGameSignature<'a>) =
  PauseMenu.start_menu "Pac Man" @"PacMan\screen.png" start_game start_selector//(start_game start_selector : StartGameSignature<World>)

and start_game_over_menu(start_selector : StartGameSignature<'a>) =
  MenuGameOver.start_menu "Pac Man" @"PacMan\screen.png" start_game start_selector


