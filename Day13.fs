module Day13

open System.IO

type Coord = { X: int; Y: int }

type Fold =
  | X of int
  | Y of int

let parse filename =
    let lines = File.ReadAllLines filename
    let i = lines |> Array.findIndex (fun x -> x = "")

    let coordinates = lines.[0 .. i - 1]
                      |> Array.map(fun x ->
                        let parts = x.Split "," |> Array.map int
                        { X = parts.[0]; Y = parts.[1]}
                      )
                      |> Set.ofArray

    let folds = lines.[i + 1 .. (Array.length lines) - 1]
                |> List.ofArray
                |> List.map (fun line ->
                  let parts = (line.Split " ").[2].Split "="
                  let position = int parts[1]

                  match parts[0] with
                  | "x" -> X position
                  | "y" -> Y position
                  | _ -> failwith "unknown command type"
                )

    (coordinates, folds)

let foldY coordinates foldSpot =
  let above = coordinates |> Set.filter (fun c -> c.Y < foldSpot)
  let below = coordinates |> Set.filter (fun c -> c.Y > foldSpot)

  let folded = below |> Set.map (fun c ->
    { X = c.X ; Y = foldSpot - (c.Y - foldSpot)}
  )
  Set.union above folded

let foldX coordinates foldSpot =
  let left = coordinates |> Set.filter (fun c -> c.X < foldSpot)
  let right = coordinates |> Set.filter (fun c -> c.X > foldSpot)

  let folded = right |> Set.map (fun c ->
    { X = foldSpot - (c.X - foldSpot) ; Y = c.Y}
  )
  Set.union left folded

let part1 (coordinates, folds) =
  match List.head folds with
  | X foldSpot -> foldX coordinates foldSpot
  | Y foldSpot -> foldY coordinates foldSpot
  |> Set.count

let part2 (coordinates, folds) =
  let rec loop coordinates folds =
    match folds with
    | hd :: tl ->
      match hd with
      | X foldSpot -> loop (foldX coordinates foldSpot) tl
      | Y foldSpot -> loop (foldY coordinates foldSpot) tl
    | [] -> coordinates

  let coords = loop coordinates folds
  let maxX = coords |> Set.toList |> List.map (fun c -> c.X) |> List.max
  let maxY = coords |> Set.toList |> List.map (fun c -> c.Y) |> List.max

  let paper = Array2D.init (maxY + 1) (maxX + 1) (fun r c ->
    if coords |> Set.contains { X = c; Y = r} then '#' else '.'
  )

  printf "\n"
  for r in seq {0 .. maxY} do
      printf "%A\n" (paper.[r,*] |> System.String)

let input = parse "./inputs/Day13.txt"

let result1 = part1 input
let result2 = part2 input
