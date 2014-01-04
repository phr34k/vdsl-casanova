module GameDefinition

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
open MapParser

let pacman_speed = 15.0f


type [<CasanovaEntity>] PacMan = {
    Circle : PhysicsEntity
    Sprite : Sprite
    Life   : Rule<int>
    Score  : Rule<int>
} with 
    member self.position = self.Circle.Position
    static member SpritePosition' (self:PacMan,dt:float32<s>) = 
      let l = self.Circle.Body.LinearVelocity.Length()
      if l < pacman_speed && l > 0.5f then
        let v = self.Circle.Body.LinearVelocity
        do v.Normalize()
        do self.Circle.ResetDynamics ()
        do self.Circle.ApplyLinearImpulse (Vector2<m/s>(v) * pacman_speed)
      self.Circle.Position
    static member Life' (world:World,self:PacMan,dt:float32<s>) = 
      let dead = Seq.exists ( fun g -> Vector2.Distance (!g.Position,self.position) < 45.0f<m>) world.Ghosts
      if dead then !self.Life-1 else !self.Life  
    static member Score'(world : World, self : PacMan, dt : float32<s>) =
      let is_colliding = Seq.exists(fun (b : Ball) -> Vector2<m>.Distance(self.position, b.Position) <= 20.0f<m>) world.Balls
      if is_colliding then !self.Score + 10 else !self.Score
        
and [<CasanovaEntity>] Wall = {
    Box : PhysicsEntity
    Sprite : Sprite  
  } with
    member self.position = self.Box.Position

and [<CasanovaEntity>] Ball = {
    Position : Vector2<m>
    Sprite : Sprite
}
and [<CasanovaEntity>] Fruit = {
    Position : Vector2<m>
    Sprite   : Sprite
}
and [<CasanovaEntity>]SuperBall = {
    Position : Vector2<m>
    Sprite : Sprite
}

and [<CasanovaEntity>] Ghost = {
    Position : Rule<Vector2<m>>
    Velocity : Var<Vector2<m/s>>
    Sprite   : Sprite
} with 
    static member SpritePosition' (self:Ghost,dt:float32<s>) = 
      !self.Position
    static member Position' (self:Ghost,dt:float32<s>) = 
      let p1 = !self.Position + self.Velocity.Value * dt
      let pos_list =
                    [for (y,x,l,r,u,d) in crossings do 
                      let x = ((float32(x) - 13.5f) / 13.5f) * 500.0f<m>
                      let y = ((float32(y) - 15.5f) / 15.5f) * 500.0f<m> 
                      if self.Velocity.Value.X > 0.0f<m/s> && self.Position.Value.Y=y && self.Position.Value.X<x then yield (x,y)
                      else if self.Velocity.Value.Y > 0.0f<m/s> && self.Position.Value.X=x && self.Position.Value.Y<y then yield (x,y)
                      else if self.Velocity.Value.Y < 0.0f<m/s> && self.Position.Value.X=x && self.Position.Value.Y>y then yield (x,y)
                      else if self.Velocity.Value.X < 0.0f<m/s> && self.Position.Value.Y=y && self.Position.Value.X>x then yield (x,y)
                      ]       
      if pos_list = [] then p1 else      
        let pos = List.minBy (fun (x,y) -> Vector2.Distance(Vector2.Create (x*1.0f<m>,y*1.0f<m>), !self.Position)) pos_list
        let v1 = Vector2.Normalize(Vector2.Create (fst pos,snd pos) - !self.Position)
        let v2 = Vector2.Normalize(Vector2.Create (fst pos,snd pos)-p1)
        if Vector2.Dot (v1,v2) < 0.0f then 
          self.Velocity := Vector2<m/s>.Zero
          Vector2.Create (fst pos,snd pos) else p1

and [<CasanovaWorld>] World = {
    Physics   : PhysicsWorld
    Canvas    : Canvas
    Balls     : RuleList<Ball> 
    SuperBalls: RuleList<SuperBall>  
    Ghosts    : RuleList<Ghost>
    PacMan    : PacMan
    Walls     : List<Wall>
    Hearts    : RuleList<Sprite>
    ScoreText : Text
    Fruits     : RuleList<Fruit>
}
with 
  static member Hearts' (self:World) =
    seq{
      for x=0 to !self.PacMan.Life do 
      let a=float32(x*50)
      yield Sprite.Create(self.Canvas,Vector2.Create((a*1.0f<m>)-100.0f<m>, 380.0f<m>),Vector2<pixel>(50.0f<pixel>,50.0f<pixel>),@"PacMan\heart.png")
     } |> Seq.toList
  static member Balls' (self:World) =
    [
      for b in self.Balls do
        if Vector2.Distance (b.Position,self.PacMan.position) > 20.0f<m> then yield b
    ]
  static member SuperBalls' (self:World) =
    [
      for b in self.SuperBalls do
        if Vector2.Distance (b.Position,self.PacMan.position) > 20.0f<m> then yield b
    ]
  static member Fruits' (self:World) =
    [
      for f in self.Fruits do
        if Vector2.Distance (f.Position,self.PacMan.position) > 20.0f<m> then yield f
    ]
  static member Ghosts'(self:World) =
     [for g in self.Ghosts do
       yield g          
      ]    
  static member ScoreTextStringRule (self:World)=
   "Score:\n" + string (!self.PacMan.Score)
     