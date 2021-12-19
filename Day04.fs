module Day04

open System.IO

type Spot = { Value: int; Marked: bool }

let parse filename =
    let lines = File.ReadAllLines filename
    let draws = lines.[0].Split "," |> Seq.map int

    let boards =
        seq { 2 .. 6 .. Array.length lines - 4}
        |> Seq.map (fun x ->
            lines.[x..(x + 4)]
            |> Seq.map (fun s ->
                s.Split(' ')
                |> Seq.filter (fun n -> n <> "")
                |> Seq.map (fun d -> { Value = int d ; Marked = false}))
            |> array2D)

    (draws, boards)

let markBoards (number: int) (boards: seq<Spot[,]>) =
    let markBoard (number: int) (board: Spot[,]) =
        board |> Array2D.map (fun x ->
          match x with
          | { Value = n; Marked = false}  when n = number -> { Value = number; Marked = true }
          | _ -> x
        )
    boards |> Seq.map (markBoard number)

let scoreBoard lastDraw board =
    lastDraw * (board |> Seq.cast<Spot> |> Seq.sumBy (fun x -> if x.Marked then 0 else x.Value))

let isWinner (board: Spot[,]) =
   seq {0 .. 4} |> Seq.exists (fun i -> (board[i, *] |> Seq.forall (fun x -> x.Marked)) || (board[*, i] |> Seq.forall (fun x -> x.Marked)))

let part1 (draws, boards) =
    let rec loop lastDraw draws boards =
        match boards |> Seq.tryFind isWinner  with
        | Some(x) -> scoreBoard lastDraw x
        | None -> loop (Seq.head draws) (Seq.tail draws) (markBoards  (Seq.head draws) boards)

    loop -1 draws boards

let part2 (draws, boards) =
  let rec loop lastDraw draws boards =
      let nonWinners = boards |> Seq.filter (fun x -> not (isWinner x))
      if Seq.isEmpty nonWinners then scoreBoard lastDraw (Seq.head boards)
      else loop (Seq.head draws) (Seq.tail draws) (markBoards  (Seq.head draws) nonWinners)
  loop -1 draws boards

let input = parse "./inputs/Day04.txt"
let result1 = part1 input
let result2 = part2 input
