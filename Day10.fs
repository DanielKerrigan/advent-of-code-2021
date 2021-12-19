module Day10

open System.IO

let parse filename =
    File.ReadAllLines filename
    |> Array.map Seq.toList

let pairs =
  [ ('{', '}')
    ('}', '{')
    ('(', ')')
    (')', '(')
    ('[', ']')
    (']', '[')
    ('<', '>')
    ('>', '<') ]
  |> Map.ofSeq

let part1 input =
  let scores =
    [ (')', 3)
      (']', 57)
      ('}', 1197)
      ('>', 25137) ]
    |> Map.ofSeq

  let rec loop stack line =
    match line with
    | head :: tail when head = '(' || head = '{' || head = '[' || head = '<' -> loop (head :: stack) tail
    | head :: tail when head = ')' || head = '}' || head = ']' || head = '>' ->
      match stack with
      | headStack :: tailStack when pairs[head] = headStack -> loop tailStack tail
      | _ :: _ -> scores[head]
      | [] -> 0
    | [] -> 0
    | _ -> failwith "unexpected character"

  input |> Array.sumBy (fun line -> loop [] line)

let part2 input =
  let scores =
    [ (')', 1L)
      (']', 2L)
      ('}', 3L)
      ('>', 4L) ]
    |> Map.ofSeq

  let rec loop stack line =
    match line with
    | head :: tail when head = '(' || head = '{' || head = '[' || head = '<' -> loop (head :: stack) tail
    | head :: tail when head = ')' || head = '}' || head = ']' || head = '>' ->
      match stack with
      | headStack :: tailStack when pairs[head] = headStack -> loop tailStack tail
      | _ :: _ -> 0L
      | [] -> failwith "complete line"
    | [] -> stack |> List.fold (fun total x -> (total * 5L) + scores[pairs[x]]) 0L
    | _ -> failwith "unexpected character"

  let totals = input
              |> Array.map (fun line -> loop [] line)
              |> Array.filter (fun x -> x <> 0)
              |> Array.sort

  totals[Array.length totals / 2]

let input = parse "./inputs/Day10.txt"
let result1 = part1 input
let result2 = part2 input
