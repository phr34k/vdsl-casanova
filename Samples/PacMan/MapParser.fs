module MapParser

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


let map = @" 
+-----------+ +-----------+
|...........| |...........|
|.+--+.+--+.| |.+--+.+--+.|
|.|  |.|  |.| |.|  |.|  |.|
|.+--+.+--+.+-+.+--+.+--+.|
|.........................|
|.+--+.+-+.+---+.+-+.+--+.|
|.+--+.| |.++ ++.| |.+--+.|
|......| |..| |..| |......|
+----+.| ++.| |.++ |.+----+
     |.| ++.+-+.++ |.|     
     |.| |.......| |.|     
+----+.+-+.+   +.+-+.+----+
|..........|   |..........|
+----+.+-+.+---+.+-+.+----+
     |.| |.......| |.|     
     |.| ++.+-+.++ |.|     
+----+.| ++.| |.++ |.+----+
|......| |..| |..| |......|
|.+--+.| |.++ ++.| |.+--+.|
|.+--+.+-+.+---+.+-+.+--+.|
|.........................|
|.+--+.+--+.+-+.+--+.+--+.|
|.|  |.|  |.| |.|  |.|  |.|
|.+--+.+--+.| |.+--+.+--+.|
|...........| |...........|
+-----------+ +-----------+ ".TrimStart()

let chars_matrix = 
  [|
    for l in map.Split([|'\n'|]) do
      yield 
        [|
          for x in l do yield x
        |]
  |]

let rec get_horizontal_walls_line y i =
  function
  | [] -> []
  | '+' :: xs ->
    let l = xs |> Seq.findIndex ((=) '+')
    let xs' = xs |> Seq.skipWhile ((<>) '+')
                  |> Seq.skip 1
                  |> Seq.toList
    (y,i,l+2) :: get_horizontal_walls_line y (i+l+2) xs'
  | _ :: xs -> get_horizontal_walls_line y (i+1) xs

let get_horizontal_walls (m:char[][]) = 
  [ for l,i in m |> Seq.mapi (fun i l -> l,i) do
      yield! get_horizontal_walls_line i 0 (l |> Seq.toList) ]

let transpose (m:char[][]) =
  [|
    for j = 0 to m.[0].Length - 1 do
      yield 
        [|
          for i = 0 to m.Length - 1 do
            yield m.[i].[j]
        |]
  |]

let transposed_char_matrix = transpose chars_matrix

let dots = 
  [|
    for j = 0 to chars_matrix.[0].Length - 1 do
      for i = 0 to chars_matrix.Length - 1 do
        if chars_matrix.[i].[j] = '.'then
          yield i,j
  |]

let crossings = 
  [|
    for j = 0 to chars_matrix.[0].Length - 1 do
      for i = 0 to chars_matrix.Length - 1 do
        if chars_matrix.[i].[j] = '*'||chars_matrix.[i].[j] = '.' then
          let up,down,left,right = 
                chars_matrix.[i].[j-1] = '.',
                chars_matrix.[i].[j+1] = '.',
                chars_matrix.[i-1].[j] = '.',
                chars_matrix.[i+1].[j] = '.'
          yield i,j,up,down,left,right
  |]

let h_walls = get_horizontal_walls chars_matrix
let v_walls = get_horizontal_walls transposed_char_matrix

