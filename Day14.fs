module Day14

open System.IO

let parse filename =
    let lines = File.ReadAllLines filename
    let template = lines.[0] |> Seq.toList

    let rec loop rules (matches: List<string>) =
      match matches with
      | hd :: tl ->
        let pair = (hd.[0], hd.[1])
        let sub = hd.[6]
        loop (rules |> Map.add pair sub) tl
      | [] -> rules

    let rules = loop Map.empty (List.ofArray lines.[2..])

    template, rules

let part1 (template, rules) =
  // one round of insertions
  let insert (template: list<char>) (rules: Map<(char * char), char>) =
    let updatedTemplate =
      // loop over the template in overlapping pairs
      (template.[0 .. (List.length template) - 2], template.[1..])
      ||> List.map2 (fun a b -> [rules[(a, b)]; b])
      |> List.concat

    List.head template :: updatedTemplate

  let rec loop i template =
    if i = 10 then template
    else loop (i + 1) (insert template rules)

  let finalTemplate = loop 0 template

  // count the number of each character in the list
  let counts =
    finalTemplate
    |> List.groupBy (fun x -> x)
    |> List.map (fun (c, chars) -> (c, List.length chars))
    |> List.sortBy ( fun (_, len) -> len)

  // difference between highest and lowest counts
  (counts |> List.last |> snd) - (counts |> List.head |> snd)

let part2 ((template: list<char>), (rules: Map<(char * char), char>)) =
  // one round of insertions
  let insert counts =
    (Map.empty, counts) ||> Map.fold (fun state k v ->
      let (a, c) = k
      let b = rules[k]
      let update x =
        match x with
        | Some s -> Some (s + v)
        | None -> Some v
      state
      |> Map.change (a, b) update
      |> Map.change (b, c) update
    )

  let rec loop i counts =
    if i = 40 then counts
    else loop (i + 1) (insert counts)

  // count the number of times each pair occurs in the starting template
  let initialCounts =
    (template.[0 .. (List.length template) - 2], template.[1..])
    ||> List.map2 (fun a b -> ((a, b), 1L))
    |> List.groupBy (fun x -> fst x)
    |> List.map (fun (pair, counts) -> (pair, counts |> List.sumBy (fun (_, v) -> v)))
    |> Map.ofList

  let finalCounts =
    loop 0 initialCounts
    |> Map.toList
    // to avoid double counting characters, we only count
    // the second one in the pair.
    |> List.map (fun (k, v) -> (snd k, v))
    |> List.groupBy (fun (k, _) -> k)
    |> List.map (fun (c, amounts) -> (c, amounts |> List.sumBy (fun x -> snd x)))
    // since we only counted the second character in a pair, we need to
    // explicitly count the first character in the template
    |> List.map (fun (c, count) -> if c = (List.head template) then (c, count + 1L) else (c, count))
    |> List.sortBy (fun (_, count) -> count)

  (finalCounts |> List.last |> snd) - (finalCounts |> List.head |> snd)

let input = parse "./inputs/Day14.txt"

let result1 = part1 input
let result2 = part2 input
