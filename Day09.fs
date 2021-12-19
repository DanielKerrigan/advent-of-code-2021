module Day09

open System.IO

let parse filename =
    File.ReadAllLines filename
    |> Array.map (fun line ->
        line
        |> Seq.map (fun c -> int c - (int '0'))
        |> Seq.toArray
    )
    |> array2D

let part1 input =
  let rows = Array2D.length1 input
  let cols = Array2D.length2 input

  let risk height neighbors =
      let isLow =
        neighbors
        |> Array.filter (fun (r, c) -> r >= 0 && c >= 0 && r < rows && c < cols)
        |> Array.forall (fun (r, c) -> input[r,c] > height)

      if isLow then height + 1 else 0

  input
  |> Array2D.mapi (fun r c height -> risk height [|(r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1)|])
  |> Seq.cast<int>
  |> Seq.sum

let part2 input =
  let rows = Array2D.length1 input
  let cols = Array2D.length2 input

  let mutable grid = input

  let rec fill r c =
    if r < 0 || c < 0 || r >= rows || c >= cols then 0
    elif grid[r,c] = -1 || grid[r,c] = 9 then 0
    else
      grid[r,c] <- -1
      1 + (fill (r + 1) c) + (fill (r - 1) c) + (fill r (c + 1)) + (fill r (c - 1))

  input
  |> Array2D.mapi (fun r c _ -> fill r c)
  |> Seq.cast<int>
  |> Seq.filter (fun x -> x <> 0)
  |> Seq.sort
  |> Seq.rev
  |> Seq.take 3
  |> Seq.reduce (*)

let input = parse "./inputs/Day09.txt"
let result1 = part1 input
let result2 = part2 input
