module Entities

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


/// The game world contains:
/// * the canvas for the stretch-free alignment of the game sprites
/// * the background texture
/// * the score text
/// * the game information ("You lose!")
/// ...
type [<CasanovaWorld>] World = {
  Canvas              : Canvas
  Background          : Sprite
  ScoreAppereance     : Text
  GameInfo            : Text
  Timer               : Rule<float32<s>>
  Movable_Figure      : Figure
  Immovable_Blocks    : RuleTable<ImmovableBlock>
  Will_Collide        : Rule<bool>
  Borders             : List<ImmovableBlock>
  Score               : Rule<float32>
  Difficulty          : Rule<float32>  
  } with

  /// Show the current score of the game and the number of deleted lines.
  static member ScoreAppereanceStringRule(self:World) =
    "Score:\n" + (!self.Score |> string) + "\nLines:\n" + (!self.Difficulty |> string)

  /// If there exists a single stack that is too high, show the "You lose" string
  static member GameInfoStringRule (world:World, dt:float32<s>) =
    if (Seq.exists (fun (immblock : ImmovableBlock) -> immblock.Position.Y < -500.0f<m>) !world.Immovable_Blocks) then
          "You lose!"
    else ""

  /// Timer that decides when to generate the next block. The speed of the timer increases
  /// the more blocks are eliminated.
  static member TimerRule (world:World, dt: float32) = 
    if !world.Timer > 1.0f<s> then
        0.0f<s>
    else !world.Timer + dt * 1.f<_> * max 1.0f ((sqrt (!world.Difficulty + 1.0f)) / 2.5f)
      
  /// Add to the bottom of the playing field the falling figure if it has reached the bottom, 
  /// and remove those rows that are completed.
  static member Immovable_BlocksRule (world:World, dt: float32) =
      if !world.Will_Collide && !world.Timer > 1.0f<s> then 
          seq{
            yield! !world.Immovable_Blocks
            for b in world.Movable_Figure.Blocks do
              yield ImmovableBlock.Create (world.Canvas,
                                            Vector2.Create((!b.RelativePosition).X,
                                                          (!b.RelativePosition).Y)
                                                      + !world.Movable_Figure.Center * 1.0f<_>,
                                            !b.Sprite.Size,
                                            !b.Sprite.Rotation,
                                            !b.Sprite.Origin,
                                            b.Sprite.Path,
                                            !b.Sprite.Color)
              }
      else
        let dictionary = Seq.groupBy (fun b -> b.Position.Y) !world.Immovable_Blocks
        if Seq.exists (fun elem -> Seq.length (snd elem) = 11) dictionary then
          let not_in_full_lines =
                seq{
                    for (line, blocks) in dictionary do
                      if (Seq.length blocks) < 11 then
                        yield! blocks
                  }
          let rows_to_be_cancel =
              seq{
                  for (line, blocks) in dictionary do
                    if (Seq.length blocks) = 11 then
                      yield line
                }
          seq{
              for b in not_in_full_lines do
                  let length = Seq.filter(fun row -> row > b.Position.Y) rows_to_be_cancel |> Seq.length                
                  
                  let fall = world.Movable_Figure.Dimension * float32 length

                  yield ImmovableBlock.Create (b.Sprite.Canvas,
                                                Vector2.Create(b.Position.X,
                                                              b.Position.Y + fall),
                                                !b.Sprite.Size,
                                                !b.Sprite.Rotation,
                                                !b.Sprite.Origin,
                                                b.Sprite.Path,
                                                b.Color)
            }
        else !world.Immovable_Blocks

  /// Check if the falling figure has touched one of the immovable blocks.
  static member Will_CollideRule (world:World, dt:float32<s>) : bool =
      let actual_position b = Vector2<m>.Create ((!world.Movable_Figure.Center).X + (!b.RelativePosition).X,
                                                 (!world.Movable_Figure.Center).Y + (!b.RelativePosition).Y)
      
      let exists_in_column immblocklist = 
                                  Seq.exists 
                                    (fun (b : Block) ->
                                      Seq.exists 
                                        (fun (closeimblock : ImmovableBlock) -> closeimblock.Position.Y >= (actual_position b).Y + world.Movable_Figure.Dimension)
                                        (Seq.filter (fun imblock -> 
                                                                  Vector2<m>.Distance(actual_position b, imblock.Position) <= world.Movable_Figure.Dimension)
                                            immblocklist))            
                                    world.Movable_Figure.Blocks
      if exists_in_column world.Borders || exists_in_column !world.Immovable_Blocks then
        let res = true
        res
      else false
          
  /// Increase the score by the number of removed lines.
  static member ScoreRule (world:World, dt: float32) =
    let dictionary = Seq.groupBy (fun b -> b.Position.Y) !world.Immovable_Blocks
    let full_lines = Seq.filter (fun elem -> Seq.length (snd elem) = 11) dictionary 
    match Seq.length full_lines with
      |1 -> !world.Score + 1.0f
      |2 -> !world.Score + 2.5f
      |3 -> !world.Score + 4.0f
      |4 -> !world.Score + 6.0f
      |_ -> !world.Score

  /// Increase the difficulty by the number of removed lines.
  static member DifficultyRule (world:World, dt: float32) =
    let dictionary = Seq.groupBy (fun b -> b.Position.Y) !world.Immovable_Blocks
    let full_lines = Seq.filter (fun elem -> Seq.length (snd elem) = 11) dictionary 
    if not (Seq.isEmpty full_lines) then 
      !world.Difficulty + float32 (Seq.length full_lines)
    else !world.Difficulty


/// An immovable block is a single block that has reached the bottom of the playing field.
/// It has a position, a color, and a single sprite for appearance.
and [<CasanovaEntity>] ImmovableBlock = {
  Position             : Vector2<m>
  Color                : Color
  Sprite               : Sprite
  } with
  /// Create an immovable block at a certain position of the game world and with a given color.
  /// TODO: signature should be (canvas: Canvas, position:Vector2<pixel>, color:Color)
  static member Create (canvas: Canvas, position:Vector2<pixel>, size:Vector2<pixel>, 
                        rotation:float32<rad>, origin:Vector2<pixel>, path:string, color:Color) =
    {
      Position  = position * 1.f<_>
      Color     = color
      Sprite    = 
        Sprite.Create(canvas, position, size, rotation,
                          origin,
                          Rectangle<pixel>.Create(-Vector2<pixel>.One * 0.5f, Vector2<pixel>.One * 0.5f),
                          path,
                          color, true
                        )
    }


/// The figure, which is only one in the game world, is the container of a series of
/// moving blocks. The figure has a center, a size, a list of blocks (each positioned
/// relatively to the figure center), and a color.
and [<CasanovaEntity>] Figure = {
  Center              : Rule<Vector2<m>>
  Dimension           : float32<m>
  Blocks              : RuleList<Block>
  Color               : Color
  } with

  /// The center of the figure falls until a collision with the bottom of the pile occurs.
  static member CenterRule (world:World, self:Figure, dt:float32<s>) =
    if !world.Will_Collide && !world.Timer > 1.0f<s> then !self.Center
    elif !world.Timer > 1.0f<s> then
      Vector2<m>.Create((!self.Center).X, (!self.Center).Y + world.Movable_Figure.Dimension)
    elif Seq.isEmpty !self.Blocks then
      Vector2.Create(0.0f<m>, -600.0f<m>)
    else !self.Center

  /// When a collision happens, we reset the blocks of the figure.
  static member BlocksRule (world:World, self:Figure, dt:float32<s>) =
    if !world.Will_Collide && !world.Timer > 1.0f<s> then
      Seq.empty
    elif (Seq.isEmpty !self.Blocks) then
      Shapes.create_random_figure(fun (p,s,c) -> Block.Create(p,s,c,world.Canvas))
    else !self.Blocks
     

/// A block simply has a position which is relative to the falling figure, 
/// and a sprite.
and [<CasanovaEntity>] Block = {
  RelativePosition    : Var<Vector2<m>>
  Sprite              : Sprite
  } with
  /// The sprite position is the block aboslute position, figure.Center + self.RelativePosition
  static member SpritePositionRule (world:World, self:Block, dt:float32<s>) =
    !self.RelativePosition + (!world.Movable_Figure.Center)

  /// Create a block with a relative position, a drawing position, a color.
  static member Create (relpos: Var<Vector2<m>>, position:Vector2<m>, color:Color, canvas: Canvas) =
      {
          RelativePosition  = relpos
          Sprite            = 
                        Sprite.Create(canvas,
                                              position * 1.f<_>,
                                              Vector2.Create(50.0f),
                                              0.0f<_>,
                                              Vector2.Create(0.0f),
                                              Rectangle<pixel>.Create(-Vector2<pixel>.One * 0.5f, Vector2<pixel>.One * 0.5f),
                                              @"Tetris/prism.png",
                                              color, true)
      }
