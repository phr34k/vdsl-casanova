module RTSgame.StarSystems
open Casanova
open Casanova.Game
open Casanova.Core
open Casanova.Drawing
open Casanova.Game
open Casanova.Input
open Casanova.Utilities
open Casanova.Math
open Casanova.StandardLibrary.Core
open Casanova.StandardLibrary.Physics
open Casanova.RTS
open RTSgame.DrawableEntities

type [<CasanovaWorld>] StarSystem =
  { 
    Layer         : Layers     
    Planets       : List<Planet>
    Fleets        : RuleList<Fleet>    
  } with
  static member FleetsRule(self : StarSystem) =    
    seq{
      for p in self.Planets do
        for n in 1..(int !p.Fleets) do
          yield Fleet.CreateFrom(!(!p.Owner), 1.0f, p.Position, "fleet.png", self.Layer.Fleets)
      for f in self.Fleets do 
        if !f.Life > 0.0f && not(self.Planets |> Seq.exists (fun p -> p.IsOrbiting(f))) then
          yield f }

and [<CasanovaEntity>] Fleet = 
  {
    Physics     : PhysicalEntity
    Owner       : Ref<Player>
    Laser       : float32
    Life        : Var<float32>
    Appearance  : DrawableSprite
    Fight       : FightAction
  } with
  member fleet.Position = !fleet.Physics.Position
  static member AppearancePositionRule(self:Fleet) = self.Position
  static member CreateFrom(owner : Player, laser, planet_position : Vector2<m>, texture, layer) : Fleet =    
    {
      Physics     = PhysicalEntity.Create(planet_position, 500.0f<m/s>)
      Owner       = ref owner
      Laser       = laser
      Life        = 1.0f |> var
      Appearance  = DrawableSprite.Create(layer, planet_position * Vector2.One,
                                          Vector2<pixel>.One * 0.05f, texture)
      Fight       = FightAction
    }

and [<CasanovaEntity>] Planet =
  {
    Position      : Vector2<m>
    Owner         : Rule<Ref<Player>>
    Appearance    : DrawableSprite
    Fleets        : Rule<float32>
    BuildAction   : BuildAction
    Minerals      : Var<float32>
  } with
  member self.IsOrbiting(f:Fleet) =
    Vector2.Distance(self.Position, f.Position) <= 10.0f<m> &&
    Vector2.Distance(self.Position, !f.Physics.Target) <= 10.0f<m>
  member self.OrbitingFleets(star_system : StarSystem) =
    seq{ for f in !star_system.Fleets do 
            if self.IsOrbiting f then yield f }  
  static member FleetsRule(star_system : StarSystem, self : Planet, dt : float32<s>) = 
    min !self.Fleets 0.0f
  static member OwnerRule(self : Planet, star_system : StarSystem) =
    let orbiting_owners = Set.ofSeq(seq{ for f in self.OrbitingFleets(star_system) do yield f.Owner })
    if !self.Fleets > 0.0f || Set.isEmpty orbiting_owners || Set.contains !self.Owner orbiting_owners then
       !self.Owner
    else
      orbiting_owners.MinimumElement
  static member Create(position, layer, texture, owner) =
    {
      Position    = position 
      Appearance  = DrawableSprite.Create(layer, position * Vector2.One,
                                          Vector2<pixel>.One * 0.1f, texture)
      Owner         = owner
      Fleets        = Rule.Create 0.0f
      BuildAction   = BuildAction
      Minerals      = var 0.0f
    }

and [<CasanovaEntity>] Player = {
  Name          : string
} with 
  static member Create(name) =
    {
      Name = name
    }

and [<RTS.Action(Target = "Any");
       RTS.Restrict(Condition = RTS.Condition.NotEqual, Field = "Owner");
       RTS.Transfer(Operation = RTS.Operation.Subtract, From = "Laser", To = "Life");
       RTS.Radius(100.0f) >] FightAction = FightAction

and [<RTS.Action(Target = "Self");
      RTS.Transfer(Operation = RTS.Operation.Add, From = "1.0f", To = "Minerals");
      RTS.Threshold(Field = "Minerals", Value = "1.0")>] BuildAction = BuildAction

