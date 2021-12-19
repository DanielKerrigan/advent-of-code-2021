module Day05

open System.IO

type Orientation =
    | Horizontal
    | Vertical
    | Diagonal

type Coord = { X: int; Y: int }

type Line =
    { A: Coord
      B: Coord
      Type: Orientation }

let parse filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map (fun x ->
        let splits = x.Split(" ")
        let first = splits.[0].Split(",") |> Array.map int
        let second = splits.[2].Split(",") |> Array.map int
        let coord1 = { X = first.[0]; Y = first.[1] }
        let coord2 = { X = second.[0]; Y = second.[1] }

        match (coord1, coord2) with
        | ({ X = x1; Y = _ }, { X = x2; Y = _ }) when x1 = x2 ->
            { A = coord1
              B = coord2
              Type = Vertical }
        | ({ X = _; Y = y1 }, { X = _; Y = y2 }) when y1 = y2 ->
            { A = coord1
              B = coord2
              Type = Horizontal }
        | _ ->
            { A = coord1
              B = coord2
              Type = Diagonal })

let part1 input =
    let rec fill counts spots =
      let increment x =
        match x with
        | Some s -> Some (s + 1)
        | None -> Some 1
      match spots with
      | [] -> counts
      | head :: rest -> fill (Map.change head increment counts) rest

    let rec loop counts lines =
        match lines with
        | [] ->
            counts
            |> Map.filter (fun _ v -> v >= 2)
            |> Map.count
        | line :: rest ->
            match line.Type with
            | Horizontal ->
              let y = line.A.Y
              let xMin, xMax = if line.A.X < line.B.X then (line.A.X, line.B.X) else (line.B.X, line.A.X)
              let spots = seq {xMin .. xMax} |> Seq.map (fun x -> {X = x; Y = y}) |> List.ofSeq
              loop (fill counts spots) rest
            | Vertical ->
              let x = line.A.X
              let yMin, yMax = if line.A.Y < line.B.Y then (line.A.Y, line.B.Y) else (line.B.Y, line.A.Y)
              let spots = seq {yMin .. yMax} |> Seq.map (fun y -> {X = x; Y = y}) |> List.ofSeq
              loop (fill counts spots) rest
            | Diagonal -> failwith "invalid line type"

    let horizonalVertical =
        input
        |> List.filter (fun d -> d.Type = Vertical || d.Type = Horizontal)

    loop Map.empty<Coord, int> horizonalVertical

let part2 input =
  let rec fill counts spots =
    let increment x =
      match x with
      | Some s -> Some (s + 1)
      | None -> Some 1
    match spots with
    | [] -> counts
    | head :: rest -> fill (Map.change head increment counts) rest

  let rec loop counts lines =
      let minMax a b = if a < b then (a, b) else (b, a)
      match lines with
      | [] ->
          counts
          |> Map.filter (fun _ v -> v >= 2)
          |> Map.count
      | line :: rest ->
          let steps a b =
              if a <= b then seq {a .. b} else {a .. -1 .. b}

          match line.Type with
          | Horizontal ->
            let y = line.A.Y
            let spots = steps line.A.X line.B.X |> Seq.map (fun x -> {X = x; Y = y}) |> List.ofSeq
            loop (fill counts spots) rest
          | Vertical ->
            let x = line.A.X
            let spots = steps line.A.Y line.B.Y |> Seq.map (fun y -> {X = x; Y = y}) |> List.ofSeq
            loop (fill counts spots) rest
          | Diagonal ->
            let xs = steps line.A.X line.B.X
            let ys = steps line.A.Y line.B.Y
            let spots = Seq.zip xs ys |> Seq.map (fun (x, y) -> {X = x; Y = y}) |> List.ofSeq
            loop (fill counts spots) rest

  loop Map.empty<Coord, int> input

let input = parse "./inputs/Day05.txt"
let result1 = part1 input
let result2 = part2 input
