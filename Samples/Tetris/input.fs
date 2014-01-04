module Input

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
open Entities


/// For scripts we redefine the (!) operator to ignore double buffering for rules.
/// This line must precede any script declaration.
let inline (!) x = immediate_lookup x


/// Get the list of input scripts.
let get_input_scripts (world:World, args:StartGameArgs, start_pause_menu) =
    // Temporary functions are inlined to avoid serialization issues that break savegames.

    /// Get the absolute screen position of a block.
    let inline actual_position b = Vector2<m>.Create ((!world.Movable_Figure.Center).X + (!b.RelativePosition).X,
                                                (!world.Movable_Figure.Center).Y + (!b.RelativePosition).Y)

    /// Find all the (immovable) blocks that are at collision distance from a specific block b.
    let inline close_blocks b immblocklist = 
          Seq.filter (fun (imblock : ImmovableBlock) -> 
                Vector2<m>.Distance((actual_position b), imblock.Position) <= world.Movable_Figure.Dimension)
              immblocklist

    /// Find if there exists a colliding block.
    let inline exists_at check k immblocklist =
          Seq.exists (fun (b : Block) -> 
                        Seq.exists
                          (fun (closeimblock : ImmovableBlock) ->
                                  check(closeimblock.Position.X - (actual_position b).X) (k * world.Movable_Figure.Dimension))
                          (close_blocks b immblocklist))
            world.Movable_Figure.Blocks

    [
      /// Accelerate the timer very quickly whenever the Down key is pressed. This makes blocks fall faster.
      wait_key_down Keys.Down => 
        co{
            world.Timer := !world.Timer + 1.0f<s>
          }

      /// Move the center of a figure left by the size of a block, but only if no collision occurs.
      wait_key_down Keys.Left =>
        co{
            let exists_at_left immblocklist = exists_at (<=) -1.0f immblocklist

            if (not (exists_at_left world.Borders || exists_at_left world.Immovable_Blocks)) then
              world.Movable_Figure.Center := !world.Movable_Figure.Center - Vector2<m>.Create(50.0f<m>, 0.0f<m>)
            do! wait_key_up Keys.Left .||> wait 0.1f<s>
        }

      /// Move the center of a figure right by the size of a block, but only if no collision occurs.
      wait_key_press Keys.Right =>
        co{
            let exists_at_right immblocklist = exists_at (>=) 1.0f immblocklist

            if (not (exists_at_right world.Borders || exists_at_right world.Immovable_Blocks)) then
              world.Movable_Figure.Center := !world.Movable_Figure.Center + Vector2<m>.Create(50.0f<m>, 0.0f<m>)
        }

      /// Rotate the relative positions of the blocks of the current figure by pi/2. For each block, its relative position X,Y becomes Y,-X.
      /// Only allow the rotation if the new relative positions will not collide.
      wait_key_press Keys.Up =>
        co{
             let rotated_position b = Vector2<m>.Create ((!world.Movable_Figure.Center).X + (!b.RelativePosition).Y,
                                                        (!world.Movable_Figure.Center).Y - (!b.RelativePosition).X)
             
             let will_collide b im_block_list =
                  Seq.exists (fun imblock -> 
                                  Vector2<m>.Distance(rotated_position b, imblock.Position) < world.Movable_Figure.Dimension)
                      im_block_list
             
             if not (Seq.exists (fun b -> will_collide b (!world.Immovable_Blocks)) world.Movable_Figure.Blocks) then
               if not (Seq.exists (fun b -> will_collide b world.Borders) world.Movable_Figure.Blocks) then
                 for b in world.Movable_Figure.Blocks do
                    b.RelativePosition := Vector2<m>.Create((!b.RelativePosition).Y, -(!b.RelativePosition).X)

        }

      /// Activate the pause menu. Pushing a new game on the stack pauses the current one.
      wait_key_press Keys.Escape => args.PushStack (start_pause_menu, false)
    ]
