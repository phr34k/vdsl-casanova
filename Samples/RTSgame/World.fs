module RTS.World

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
open RTS.StarSystems

/// The game world handles general services around turn management and selection.
/// The game also contains:
/// -the layers that we draw onto
/// -the star system where gameplay happens
/// -the background picture
/// -the camera
/// -the turn manager that handles the current player
/// -the selection manager that handles selection
type [<CasanovaWorld>] World = {
  Layers      : Layers
  StarSystem  : StarSystem
  Players     : Player[]  
  Background  : Sprite
  Camera      : Camera2D  
  TurnManager : TurnManager
  Selection   : SelectionManager
} with
  /// Create initializes a game world with some input players.
  static member Create(players, device, content) =
    /// We start by setting up the rendering layers; all layers use alpha blending to some
    /// extent, besides the background layer which is the only one that uses the BlendState.Opaque
    /// flag.
    let layers : Layers = 
      {
        Background      = SpriteLayer.Create(device, content, BlendState.Opaque)
        Planets         = SpriteLayer.Create(device, content)
        Fleets          = SpriteLayer.Create(device, content)
        Overlay         = SpriteLayer.Create(device, content)        
        Gui             = SpriteLayer.Create(device, content)
      }
    /// The background is loaded from a texture, and is drawn at the center of the screen; its surface (2000*2000) is four times bigger
    /// than the visible surface of the screen (1000*1000)
    let background = Sprite.Create(layers.Background, Vector2.Zero, Vector2.One * 2000.0f, @"RTS\Backgrounds\background")
    /// The camera is constrained to not move beyond the -1000,+1000 range along each axis; its zoom level goes from 2x to 0.2x
    let camera = Camera2D.Create(Camera2DBounds.Create(-1000.0f<m>, 1000.0f<m>, -1000.0f<m>, 1000.0f<m>, 0.5f<m>, 5.0f<m>))
    /// Players, star systems, selection, and turn manager initialize themselves.
    let players = players |> Array.map Player.Create
    let star_systems = StarSystem.Create(players, layers)
    let selection_manager = SelectionManager.Create()
    let turn_manager = TurnManager.Create(players, layers)
    {
      StarSystem    = star_systems 
      Players       = players
      Background    = background
      Layers        = layers
      Camera        = camera
      Selection     = selection_manager
      TurnManager   = turn_manager
    }
  /// We propagate the current transformation matrix of the camera to the gameplay layers, 
  /// that is all of them besides the background and GUI layers.
  static member LayersPlanetsTransformRule(world:World, dt:float32<s>) = world.Camera.Transform
  static member LayersFleetsTransformRule(world:World, dt:float32<s>) = world.Camera.Transform
  static member LayersOverlayTransformRule(world:World, dt:float32<s>) = world.Camera.Transform

/// The turn manager stores the current turn owner (a player ref, since it is not the actual
/// player bt only a pointer to it), plus a picture and some text to draw the name of the
/// current player and the remaining time for his turn.
and [<CasanovaEntity>] TurnManager = {
  TurnCanvas                  : Canvas
  CurrentTurnOwner            : Rule<Ref<Player>>
  CurrentTurnOwnerBackground  : Sprite
  CurrentTurnOwnerName        : ShadowedText
  Timer                       : Timer
} with 
  static member Create(players, layers) =
    let turn_canvas = Canvas.Create(layers.Gui, -Vector2.UnitY * 500.f, Vector2.Create(611.f,136.67f), Vector2<1>.One, Vector2<pixel>.UnitY * -0.5f, StretchMode.Uniform)
    {
      TurnCanvas                  = turn_canvas
      /// the turn owner is the first player of the list
      CurrentTurnOwner            = Rule.Create (ref (players |> Seq.head))
      /// The drawable text is on the upper left corner (-490,-490) of the screen; it is 100x50 pixels
      CurrentTurnOwnerName        = ShadowedText.Create(turn_canvas, Vector2<pixel>.UnitY * -250.0f, "", Vector2<pixel>.One * 3.5f, Vector2.Create(245.f,426.f) * 0.75f, Color.Blue, Color.White)
      /// The background sprite for the current turn text is simply a white pixel, stretched from the upper 
      /// left corner of the screen to cover the current turn text.
      CurrentTurnOwnerBackground  = Sprite.Create(turn_canvas, Vector2<pixel>.Zero, Vector2<pixel>.One * 1000.f, 0.0f<rad>, Vector2<pixel>.Zero, Rectangle.FromCenterSize(Vector2<_>.Zero, Vector2<_>.One), @"RTS\UI\timer_dock", Color.White, true)
      //CurrentTurnOwnerBackground  = Sprite.Create(layers.Gui, Vector2.Create(-500.0f,-500.0f), Vector2.Create(125.f,75.f), 0.0f<_>, -Vector2<pixel>.One * 0.5f, @"UI\white_pixel", Color.White, true)
      /// The current timer starts at 0; note that Timer is an internal Casanova utility
      Timer                       = { TotalTime = Rule.Create(0.0f<s>) }
    }
  /// A turn lasts 20 secodns
  member this.TurnDuration = 20.0f<s>
  /// With a low update frequency (five times per second), update the string that
  /// describes the current player and the amount of time left.
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]    
  static member CurrentTurnOwnerNameStringRule(world:World, self:TurnManager) =
    (!(!self.CurrentTurnOwner)).Name + ":  " +
    (sprintf "%02d" (int self.TurnDuration - (int !self.Timer.TotalTime % int self.TurnDuration)))
  /// With a low update frequency (five times per second), update the color of the 
  /// string that describes the current player and the amount of time left; the color
  /// is the same as that of the current player.
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]    
  static member CurrentTurnOwnerNameColorRule(world:World, self:TurnManager) =
    (!(!self.CurrentTurnOwner)).Color
  /// With a low update frequency (five times per second), update the current player.
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]    
  static member CurrentTurnOwnerRule(world:World, self:TurnManager, dt:float32<s>) = 
    ref world.Players.[int (!self.Timer.TotalTime / self.TurnDuration) % world.Players.Length]

/// The selection manager stores a sprite that contains the current selection rectangle,
/// and maintains a utility list of all the selected ships in the game world.
and [<CasanovaEntity>] SelectionManager = {
  Rectangle           : Var<Sprite[]>
  SelectedFleets      : RuleList<Ref<Fleet>>
} with 
  /// The selection manager is initialized empty.
  static member Create() = 
    {
      SelectedFleets      = RuleList.Empty
      Rectangle           = var([||])
    }
  /// With low frequency, since they do not change so often, we update
  /// the selected fleets; we read all the fleets in the game, and then 
  /// add them to the list if their Selected field is true
  [<RuleUpdateFrequency(UpdateFrequency.AI)>]    
  static member SelectedFleetsRule(world:World, self:SelectionManager, dt:float32<s>) =
    seq{
      for f in world.StarSystem.Fleets do
        if !f.Selected then
          yield ref f
    }
  /// When we update the selection we (re-)create the selection rectangle so that it has
  /// the appropriate size.
  member self.UpdateSelection(world,start_position:Vector2<m>,end_position:Vector2<m>) =
    let size = start_position - end_position
    self.Rectangle := 
        [|
          Sprite.Create(world.Layers.Overlay, 
              start_position * 1.0f<pixel/m>, 
              size * 1.f<pixel/m>, 0.0f<_>, Vector2<pixel>.One * 0.5f, @"UI\white_pixel", Color.CornflowerBlue * 0.5f, true)
        |]
  /// When we end the selection we resize the selection rectangle.
  member self.EndSelection() =
    self.Rectangle := [||]
  