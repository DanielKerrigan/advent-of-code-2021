module Day06

open System.IO
open System.Collections.Generic

let parse filename =
    let lines = File.ReadAllLines filename
    lines.[0].Split(",")
    |> Array.map int
    |> List.ofArray

let part1 fish =
  let step fish =
    let newFish = fish
                  |> List.filter (fun x -> x = 0)
                  |> List.map (fun _ -> 8)

    let oldFish = fish |> List.map (fun x -> if x = 0 then 6 else x - 1)

    newFish @ oldFish

  let rec loop day fish =
    if day = 0 then fish else loop (day - 1) (step fish)

  loop 80 fish |> List.length

let part2 fish =
  let cache = new Dictionary<int, int64>()

  let rec spawn days =
    if cache.ContainsKey days then cache.[days]
    else
        let result = if days < 0 then 0L else (1L + spawn (days - 7) + spawn (days - 9))
        cache.Add(days, result)
        result
  // 1 at the beginning is to count this fish
  // 1 at the end is because fish spawn day after 0
  fish |> List.sumBy (fun x -> 1L + spawn (256 - (x + 1)))

let input = parse "./inputs/Day06.txt"
// 359344
let result1 = part1 input
let result2 = part2 input
