module Day12

open System.IO

let parse filename =
    File.ReadAllLines filename
    |> Array.fold (fun graph cur ->
        let path = cur.Split "-"
        graph
        |> Map.change path.[0] (fun x ->
          match x with
          | Some s -> Some (path.[1] :: s)
          | None -> Some([path.[1]])
        )
        // add edge in other direction too
        |> Map.change path.[1] (fun x ->
          match x with
          | Some s -> Some (path.[0] :: s)
          | None -> Some([path.[0]])
        )
    ) Map.empty

let part1 graph =
  let rec traverse visited node =
    match node with
    | "end" -> 1
    | x when visited |> Set.contains x -> 0
    | x when x.[0] >= 'a' && x.[0] <= 'z' ->
      let updated = visited |> Set.add node
      graph
      |> Map.find node
      |> List.sumBy (fun n -> traverse updated n)
    | _ ->
      graph
      |> Map.find node
      |> List.sumBy (fun n -> traverse visited n)
  traverse Set.empty "start"

let part2 graph =
  let rec traverse visited visitedSmallTwice node =
    match node with
    | "end" -> 1
    | x when visitedSmallTwice && visited |> Set.contains x -> 0
    | x when not visitedSmallTwice && visited |> Set.contains x ->
      graph
      |> Map.find node
      |> List.filter (fun n -> n <> "start")
      |> List.sumBy (fun n -> traverse visited true n)
    | x when x.[0] >= 'a' && x.[0] <= 'z' ->
      let updated = visited |> Set.add node
      graph
      |> Map.find node
      |> List.filter (fun n -> n <> "start")
      |> List.sumBy (fun n -> traverse updated visitedSmallTwice n)
    | _ ->
      graph
      |> Map.find node
      |> List.filter (fun n -> n <> "start")
      |> List.sumBy (fun n -> traverse visited visitedSmallTwice n)

  traverse Set.empty false "start"

let input = parse "./inputs/Day12.txt"

let result1 = part1 input
let result2 = part2 input
