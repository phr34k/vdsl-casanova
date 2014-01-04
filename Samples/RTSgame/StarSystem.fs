module RTS.StarSystems

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
open Casanova.Action
open RTS.DrawableEntities

/// The star system is a CasanovaWorld, in that it acts as a local
/// root for all the entities it contains. Those entities do not
/// know anything about the game world and can only interact with
/// the other entities inside the star system itself.
/// The star system contains:
/// -a reference to the drawable layers; it is important that layers
///  inside the star system act as a reference, otherwise everything
///  will be rendered twice
/// -a list of planets
/// -a list of fleets
/// -a list of references to players
type [<CasanovaWorld>] StarSystem =
  { 
    Layers        : Ref<Layers>
    Planets       : List<Planet>
    Fleets        : RuleList<Fleet>    
    Players       : Ref<Player>[]
  } with    
  /// We create the star system from the players and the layers
  static member Create(players:Player[], layers) =    
    /// First of all convert all players to references
    let players = [| for p in players do yield ref p |]

    /// We initialize a randomized grid of planets from a list of possible planet textures and sizes.
    /// Bigger planets build ships faster.
    let mutable planets = []
    for i = -10 to 10 do
      for j = -10 to 10 do
        if random_interval 0 10 <= 3 then
          let texture,size = 
            [
              @"RTS\Planets\earth", 50.f
              @"RTS\Planets\findolfin.png", 30.f
              @"RTS\Planets\fungo.png", 40.f
              @"RTS\Planets\ice.png", 50.f
              @"RTS\Planets\marte.png", 40.f
              @"RTS\Planets\pluto.png", 30.f
              @"RTS\Planets\saturn.png", 70.f
              @"RTS\Planets\thor.png", 80.f
            ] |> List.RandomElement
          let new_planet = Planet.Create(Vector2.Create(i, j) * 120.0f + Vector2.Create(random_range -25.0f 25.0f, random_range -25.0f 25.0f) * 0.25f, layers, texture, size, players.[ (random_interval 0 players.Length) ])
          do planets <- new_planet :: planets
    {      
      Players       = players
      Layers        = ref layers
      Planets       = planets
      Fleets        = RuleList.Create (fun () -> Seq.empty)
    }
  /// The fleets list is updated by keeping all the fleets that are still alive, plus
  /// all the fleets from planets that just finised building; when a planet is done 
  /// building it will set, for just one frame, the NewFleet field to true.
  /// Notice that we do not build more than a given number of fleets in total.
  static member FleetsRule(self : StarSystem) =    
    seq{
      for p in self.Planets do        
        if !p.NewFleet && self.Fleets.Value |> Seq.length < 300 then
          yield Fleet.CreateFrom(!(!p.Owner), 1.0f, p.Position, @"RTS\Ships\ship", !self.Layers)
      for f in self.Fleets do 
        if !f.Life > 0.0f then
          yield f }

/// Fleets implement a large portion of the logic of the game. A fleet handles
/// basic flocking and path-finding, maintains information on its owner, 
/// handles battles, and draws its texture, selection halo, lasers, and health
/// bar.
and [<CasanovaEntity>] Fleet = 
  {
    /// Position, target, velocity and max speed
    /// determine movement for the ship.
    Position    : Rule<Vector2<m>>
    Target      : Var<Vector2<m>>
    Velocity    : Rule<Vector2<m/s>>
    MaxSpeed    : Var<float32<m/s>>

    /// Neighboring fleets contains all the nearby 
    /// fleets in order to avoid excessive superposition.
    /// The list is filled by the FindNeighboringFleetsAction,
    /// which performs spatial optimizations in order to
    /// find the neighbors quickly.
    NeighbouringFleets : RuleList<Vector2<m>>
    FindNeighbouringFleetsAction : FindNeighbouringFleetsAction

    Owner       : Ref<Player>

    /// Fighting involves the current health of the ship
    /// and the fight action which takes care of modifying 
    /// the life of ships when enemies enter in range.
    Life          : Var<float32>
    Fight         : FightAction

    /// The fight action also fills the laser targets
    /// with all the fleets that are currently being
    /// attacked; these fleets are then linked to the
    /// current one by lines that represent lasers.
    LaserTargets  : RuleList<Ref<Fleet>>
    Lasers        : RuleList<Line>

    /// The ship draws its own texture, a shadow that
    /// shows the color of the owner and the current 
    /// selection, and a line that represents the fleet's
    /// health.
    Appearance  : Sprite
    FleetShadow : Sprite
    Stats       : Line

    Selected    : Var<bool>
  } with
  static member LaserTargetsRule(self:Fleet) : seq<Ref<Fleet>> = Seq.empty
  /// Create one line for each laser target from self to the target.
  static member LasersRule(world:StarSystem, self:Fleet) = 
    seq{
      for t in self.LaserTargets do
        let t = !t.Value.Position
        yield Line.Create((!world.Layers).Overlay, !self.Position * 1.0f<_>, t * 1.0f<_>, @"UI\laser", Color.White, 1.0f)
    }
  /// When a new target is sent to this fleet, then zero the velocity and set the target.
  member inline this.SetTarget(target : Vector2<m>) =
    this.Velocity := Vector2.Create(0.0f)
    this.Target := target
  /// The health bar moves with the fleet
  static member StatsSourceRule(self:Fleet) = !self.Position + Vector2.UnitY * 10.0f - Vector2.UnitX * 5.0f
  /// The length of the health bar reflects the health; longer = healthier
  static member StatsDestRule(self:Fleet) = !self.Position + Vector2.UnitX * !self.Life + Vector2.UnitY * 10.0f - Vector2.UnitX * 5.0f
  /// The color of the health bar reflects the health; greener = healthier
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]
  static member StatsColorRule(self:Fleet) = Color.Lerp(Color.Red, Color.Green, !self.Life / 10.0f)
  /// The texture moves with the fleet
  static member AppearancePositionRule(self:Fleet) = !self.Position
  /// The shadow moves with the fleet
  static member FleetShadowPositionRule(self:Fleet) = !self.Position
  /// The shadow is bigger and darker when the ship is selected
  [<RuleUpdateFrequency(UpdateFrequency.Interactive)>]
  static member FleetShadowScaleRule(self:Fleet) =
    if !self.Selected then Vector2 1.25f else Vector2 1.0f
  [<RuleUpdateFrequency(UpdateFrequency.Interactive)>]
  static member FleetShadowColorRule(self:Fleet) =
    if !self.Selected then (!self.Owner).Color else Color.Lerp((!self.Owner).Color, Color.White, 0.25f)
  /// The velocity moves towards the target, but it also moves away from 
  /// the neighboring fleets. The neighboring fleets are filled by the 
  /// FindNeighbouringFleetsAction action with a spatial partitioning
  /// optimization.
  static member NeighbouringFleetsRule(self:Fleet) : seq<Vector2<m>> = Seq.empty
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]
  static member VelocityRule(star_system:StarSystem,self:Fleet,dt:float32<s>) =
    let target_distance = Vector2<m>.Distance(!self.Position,!self.Target)
    let repulsions = 
      seq{
        for f' in self.NeighbouringFleets do
          let strength = smoothstep(Vector2.Distance(f', !self.Position) / 50.0f<m>) 5.0f 0.0f
          let away = 
            if Vector2.Distance(!self.Position, f') <= 0.1f<m> then Vector2.Create(random_interval -1 1, random_interval -1 1) * 5.0f
            else Vector2.Normalize(!self.Position - f')
          yield away * strength
      }
    let repulsions = repulsions |> Seq.fold (+) Vector2.Zero    
    let try_normalize (v:Vector2<1>) = 
      if v.Length <= 0.1f then v
      else v |> Vector2.Normalize
    let velocity'= 
      if target_distance < 0.1f<m> then
        (repulsions |> try_normalize) * !self.MaxSpeed
      else
        (!self.Target - !self.Position).Normalized * !self.MaxSpeed + (repulsions |> try_normalize) * !self.MaxSpeed
    Vector2.Lerp(!self.Velocity, velocity', dt * 1.0f<_>)
  /// The position of a fleet is updated by its velocity, unless
  /// it reached its target.
  static member PositionRule(star_system:StarSystem,self:Fleet,dt:float32<s>) =
    let target_distance = Vector2<m>.Distance(!self.Position,!self.Target)
    let new_position = !self.Position + dt * !self.Velocity
    if target_distance < 0.1f<m>  then 
      !self.Target
    else
      new_position
  /// We create a new fleet with empty counters, loaded textures, and full life.
  static member CreateFrom(owner : Player, laser, planet_position : Vector2<m>, texture, layers : Layers) : Fleet =    
    let pos = planet_position - Vector2<m>.UnitY * 70.0f
    {
      LaserTargets        = RuleList.Create(fun () -> Seq.empty)
      NeighbouringFleets  = RuleList.Create(fun () -> Seq.empty)
      Lasers              = RuleList.Create(fun () -> Seq.empty)
      Position    = Rule.Create(planet_position + Vector2<m>.Create(random_range -1.0f 1.0f, random_range -1.0f 1.0f) * 10.0f)
      FleetShadow = Sprite.Create(layers.Planets, planet_position * Vector2.One, Vector2<pixel>.One * 70.f, @"UI\selection_frame", owner.Color)
      Target      = var(planet_position + Vector2.Create(random_range -1.0f 1.0f, random_range -1.0f 1.0f) * 50.0f)
      Velocity    = Rule.Create(Vector2.Zero)
      MaxSpeed    = var 50.0f<m/s>
      Owner       = ref owner
      Life        = var 10.0f
      Stats       = Line.Create(layers.Overlay, planet_position  * 1.0f<_>, planet_position  * 1.0f<_>, @"UI\white_pixel", Color.White, 5.0f)
      Appearance  = Sprite.Create(layers.Fleets, planet_position * Vector2.One, Vector2<pixel>.One * 50.f, texture)
      Selected    = var false
      Fight       = FightAction
      FindNeighbouringFleetsAction = FindNeighbouringFleetsAction
    }

/// Planets mainly handle building of new fleets. When an enemy fleets are the closest to
/// the planet, then it is conquered and it changes owner.
/// Building of fleets is done through the BuildAction action which accumulates
/// minerals. When the target amount of minerals is reached, then NewFleet is set to
/// true for one frame.
/// The planet also stores a position, an owner, and its visuals. The visuals are
/// made up of a texture for the planet, a bar that signals building completion,
/// and a halo that determines the planet owner.
and [<CasanovaEntity>] Planet =
  {
    Position      : Vector2<m>
    Owner         : Rule<Ref<Player>>
    Appearance    : Sprite
    PlanetShadow  : Sprite
    Stats         : Line
    NewFleet      : Rule<bool>
    BuildAction   : BuildAction
    GatherSpeed   : float32
    Minerals      : Var<float32>
    EnemyOrbitingFleets : RuleTable<Ref<Player>>
    EnemyOrbitingFleetsAction : EnemyOrbitingFleetsAction
  } with
  /// The color of the background halo of the planet is the same as the planet owner.
  /// It is updated with low frequency.
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]
  static member PlanetShadowColorRule(self:Planet) = (!(!self.Owner)).Color  
  /// The stats bar length depends on the amount of gathered minerals.
  /// It is updated with low frequency.
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]
  static member StatsDestRule(self:Planet) = self.Position + Vector2.UnitX * !self.Minerals * 2.0f + Vector2.UnitY * 10.0f - Vector2.UnitX * 10.0f
  /// The stats bar color depends on the amount of gathered minerals.
  /// It is updated with low frequency.
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]
  static member StatsColorRule(self:Planet) = Color.Lerp(Color.CornflowerBlue, Color.GreenYellow, !self.Minerals / 10.0f)

  /// The owner is changed if only enemy fleets are orbiting the planet.
  /// It is updated with low frequency.
  static member EnemyOrbitingFleetsRule(self : Planet, star_system : StarSystem) : seq<Ref<Player>> = Seq.empty
  
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]
  static member OwnerRule(self : Planet, star_system : StarSystem) =
    //let orbiting_owners = seq{ for f in self.OrbitingFleets(star_system) do yield f.Owner }
    if Seq.isEmpty self.EnemyOrbitingFleets then
       !self.Owner
    else
      Seq.head self.EnemyOrbitingFleets
  /// NewFleet is reset to false after every tick, but it is set to true by the BuildAction action.
  static member NewFleetRule(self : Planet, star_system : StarSystem) = false
  /// We create a new planet with empty counters and loaded visuals.
  static member Create(position, layers, texture, size : float32, owner : Ref<Player>) =
    {
      EnemyOrbitingFleetsAction = EnemyOrbitingFleetsAction
      EnemyOrbitingFleets = RuleTable.Create(fun () -> Seq.empty)
      Position      = position 
      Appearance    = Sprite.Create(layers.Planets, position * Vector2.One, Vector2<pixel>.One * size, texture)
      PlanetShadow  = Sprite.Create(layers.Planets, position * Vector2.One, Vector2<pixel>.One * size * 1.5f<_>, @"UI\selection_frame")
      Stats         = Line.Create(layers.Overlay, position * 1.0f<pixel/m> + Vector2.UnitY * 10.0f - Vector2.UnitX * 10.0f, position * 1.0f<pixel/m>, @"UI\white_pixel", Color.White, 5.0f)
      Owner         = Rule.Create(owner)
      NewFleet      = Rule.Create false
      BuildAction   = BuildAction
      GatherSpeed   = size / 50.0f
      Minerals      = var 0.0f
    }

/// The player simply contains a name and a color.
and [<CasanovaEntity; ReferenceEquality>] Player = {
  Name          : string  
  Color         : Color
} with static member Create(name, color) = { Name = name; Color = color}



and [<Action.Action(Target = "Fleet");
       Action.Restrict(Condition = Action.Condition.NotEqual, Field = "Owner");       
       Action.Insert(Value = "Owner", To = "EnemyOrbitingFleets");
       Action.Radius(25.0f) 
       >] EnemyOrbitingFleetsAction = EnemyOrbitingFleetsAction

/// The fight action subtracts 0.5*dt during every tick from the life of the 
/// enemy fleets that are within range. All laser targets are added to the 
/// LaserTargets list of the source fleet.
and [<Action.Action(Target = "Fleet");
       Action.Restrict(Condition = Action.Condition.NotEqual, Field = "Owner");
       Action.Transfer(Operation = Action.Operation.Subtract, From = "0.5", To = "Life");       
       Action.Insert(Value = "this", To = "LaserTargets");
       Action.Radius(150.0f) 
       >] FightAction = FightAction

/// The FindNeighbouringFleetsAction adds nearby fleets to the NeighbouringFleets
/// list, so that the flocking algorithm can avoid collisions.
and [<Action.Action(Target = "Fleet");
       Action.Insert(Value = "Position", To = "NeighbouringFleets");
       Action.Radius(25.0f) 
       >] FindNeighbouringFleetsAction = FindNeighbouringFleetsAction

/// BuildAction adds GatherSpeed*dt to Minerals inside a planet. When Minerals reaches
/// the 10.0 threshold, then NewFleets is set to true and Minerals is reset to 0.
and [<Action.Action(Target = "Self");
      Action.Transfer(Operation = Action.Operation.Add, From = "GatherSpeed", To = "Minerals");
      Action.Threshold(Field = "Minerals", Value = "10.0");
      Action.Output(Operation = Action.Operation.Set, Value = "true", To = "NewFleet");
      Action.Output(Operation = Action.Operation.Set, Value = "0.0", To = "Minerals")>] BuildAction = BuildAction
