module CellularAutomaton

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Media
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Casanova
open Casanova.Core
open Casanova.Action
open Casanova.Coroutines
open Casanova.Utilities
open Casanova.Math
open Casanova.Math.MathHelper
open Casanova.Game
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.Core

/// The game world contains the fireflies, which roam randomly and turn on and off depending on their neighbours.
type [<CasanovaWorld>] World =
  {
    Fireflies     : List<Firefly>
  }

/// A Firefly has a position, a rotation, a velocity, and an acceleration.
/// It also contains a sprite for its appearance.
/// The firefly may be on or off, depending on its value of TurnedOn; TurnedOn
/// in turn is triggered by the number of on or off neighbors, which are found
/// by the FindBeighbours action.
and [<CasanovaEntity>] Firefly = 
  {
    Position              : Rule<Vector2<pixel>>
    AngularAcceleration   : Rule<float32<rad/(s^2)>>
    AngularSpeed          : Rule<float32<rad/s>>
    Rotation              : Rule<float32<rad>>
    Sprite                : Sprite
    TurnedOn              : Rule<bool>
    Neighbours            : RuleTable<bool>
    FindNeighbours        : FindNeighbours
  }
  /// Reset the neighbours. The FindNeighbours action will replenish the list.
  static member Neighbours' (world : World, self : Firefly, dt : float32<s>) = 
    Seq.empty<bool>
  /// A firefly turns on when more than ten of its neighbors are all on or all off.
  static member TurnedOn' (world : World, self : Firefly, dt : float32<s>) = 
    (self.Neighbours |> Seq.filter id |> Seq.length > 10) ||
    (self.Neighbours |> Seq.filter not |> Seq.length > 10)
  /// Fireflies move randomly. The randomness affects their angular acceleration, which is then integrated
  /// into the position and the velocity. When a firefly touches the borders of the screen, then it stops moving.
  static member AngularAcceleration' (world : World, self : Firefly, dt : float32<s>) = 
    let a = !self.AngularAcceleration + (random_range -pi_over_2 pi_over_2) * dt * 10.f<_>
    clamp(a, -pi_over_2 * 1.f<1/s^2>, pi_over_2 * 1.f<1/s^2>)
  static member AngularSpeed' (world : World, self : Firefly, dt : float32<s>) = 
    let a_s = !self.AngularSpeed + (!self.AngularAcceleration * dt)
    clamp(a_s, -pi_over_2 * 1.f<1/s>, pi_over_2 * 1.f<1/s>)
  static member Rotation' (world : World, self : Firefly, dt : float32<s>) = 
    !self.Rotation + (!self.AngularSpeed * dt)
  static member Position' (world : World, self : Firefly, dt : float32<s>) = 
    let direction = Vector2<_>(cos !self.Rotation, sin !self.Rotation)
    let pos' = !self.Position + (direction * 50.0f<pixel/s> * dt)
    if Rectangle<pixel>.Create(Vector2<pixel>.One * -500.f, Vector2<pixel>.One * 500.f).Contains pos' then
      pos'
    else
      !self.Position
  /// The firefly position and rotation are copied to its sprite.
  static member SpriteRotationRule (world : World, self : Firefly, dt : float32<s>) = 
    !self.Rotation
  static member SpritePositionRule (world : World, self : Firefly, dt : float32<s>) = 
    !self.Position
  /// If the firefly is currently on, draw it in yellow, otherwise use gray.
  static member SpriteColorRule (world : World, self : Firefly, dt : float32<s>) = 
    if !self.TurnedOn then Color.Yellow
    else Color.Gray
  /// Create a random firefly.
  static member Create(layer : SpriteLayer) = 
    let pos = Vector2<pixel>(random_range -500.f<_> 500.f<_>, random_range -500.f<_> 500.f<_>)      
    let rotation = random_range -pi pi
    {
      Position  = Rule.Create pos
      AngularAcceleration   = Rule.Create 0.f<rad/(s^2)>
      AngularSpeed          = Rule.Create 0.f<rad/s>
      Rotation  = Rule.Create rotation
      Sprite    = Sprite.Create(layer, pos, Vector2<_>.One * 20.f<_>, @"CellularAutomaton\Firefly")
      TurnedOn  = Rule.Create false
      Neighbours = RuleTable.Empty
      FindNeighbours= FindNeighbours
    }

/// FindNeighbours finds all the fireflies within a radius of 40 units and copies their
/// TurnedOn value into the Neighbours field of the source.
and [<Action(Target = "Firefly");
      Radius(40.f);
      Insert(Value = "TurnedOn", To = "Neighbours")>] FindNeighbours = FindNeighbours

let start_game (start_selector : StartGameSignature<'a>)  (args : StartGameArgs) = 
  /// The game world contains a series of random fireflies.
  let world = 
    { 
      Fireflies     = [for i = 0 to 2000 do yield Firefly.Create(args.DefaultLayer) ] 
    }

  // For scripts we re-define the lookup operator so that we always interact with the next value 
  // of rules, and not with the current value; this way scripts act as fully imperative threads
  let inline (!) x = immediate_lookup x

  let main = yield_

  let input = 
    [
      /// Quit the game when Escape is pressed
      wait_key_press Keys.Escape  => args.PushStack(ConfirmationMenu.start_menu (args.SetStack start_selector), false) 
      /// Save and load with F9 and F10
      wait_key_press Keys.F9      => args.Save "CellularAutomaton"
      wait_key_press Keys.F10     => args.Load "CellularAutomaton"
    ]
  world, main, input
