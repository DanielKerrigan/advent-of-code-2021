module Day11

open System.IO

let parse filename =
    File.ReadAllLines filename
    |> Array.map (fun line ->
        line
        |> Seq.map (fun c -> int c - (int '0'))
        |> Seq.toArray
    )
    |> array2D

let getNeighbors r c rows cols =
  [
    (r + 1, c);
    (r - 1, c);
    (r, c + 1);
    (r, c - 1);
    (r + 1, c + 1);
    (r + 1, c - 1);
    (r - 1, c + 1);
    (r - 1, c - 1);
  ] |> List.filter (fun (row, col) -> row >= 0 && col >= 0 && row < rows && col < cols)

let part1 input =
  let rows = Array2D.length1 input
  let cols = Array2D.length2 input

  let mutable input = Array2D.copy input

  let step i =
      let mutable flashes = Set.empty

      let increase row col =
          input[row,col] <- input[row,col] + 1

      let rec flash r c =
        match (r, c) with
        | (row, col) when flashes |> Set.contains (row, col) -> ()
        | (row, col) when input[row,col] > 9 ->
          flashes <- Set.add (r, c) flashes
          input[row,col] <- 0
          let neighbors = getNeighbors row col rows cols
          neighbors |> List.iter (fun (a, b) -> if not (Set.contains (a, b) flashes) then increase a b)
          neighbors |> List.iter (fun (a, b) -> flash a b)
        | _ -> ()

      input |> Array2D.iteri (fun r c _ -> increase r c)
      input |> Array2D.iteri (fun r c _ -> flash r c)

      Set.count flashes

  Seq.sum (seq { for i in 1 .. 100 -> step i})


let part2 input =
  let rows = Array2D.length1 input
  let cols = Array2D.length2 input

  let mutable input = Array2D.copy input

  let step i =
      let mutable flashes = Set.empty

      let increase row col =
          input[row,col] <- input[row,col] + 1

      let rec flash r c =
        match (r, c) with
        | (row, col) when flashes |> Set.contains (row, col) -> ()
        | (row, col) when input[row,col] > 9 ->
          flashes <- Set.add (r, c) flashes
          input[row,col] <- 0
          let neighbors = getNeighbors row col rows cols
          neighbors |> List.iter (fun (a, b) -> if not (Set.contains (a, b) flashes) then increase a b)
          neighbors |> List.iter (fun (a, b) -> flash a b)
        | _ -> ()

      input |> Array2D.iteri (fun r c _ -> increase r c)
      input |> Array2D.iteri (fun r c _ -> flash r c)

      Set.count flashes

  let rec loop i =
    if step i = (rows * cols) then i
    else loop i + 1

  loop 1

let input = parse "./inputs/Day11.txt"

let result1 = part1 input
let result2 = part2 input
