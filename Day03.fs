module Day03

open System.IO

let binaryToDecimal numBits binarySequence =
    binarySequence |> Seq.mapi (fun i x -> x <<< (numBits - i - 1)) |> Seq.sum

// get the ith bit of x, where i = 0 is the least significant
let getBit x i =
    (x &&& (1 <<< i)) >>> i

let parse filename =
    let lines = File.ReadAllLines filename
    let numBits = String.length lines[0]
    let nums = lines |> Array.map (fun str -> str |> Seq.toArray |> Array.map (fun c -> int c - int '0') |> binaryToDecimal numBits)
    (numBits, nums)

let part1 (numBits, report) =
    let numRows = Array.length report

    let gamma = seq { (numBits - 1) .. -1 .. 0 }
                |> Seq.sumBy (fun i ->
                  let total = report |> Seq.sumBy (fun x -> getBit x i)
                  if (total > (numRows / 2)) then 1 <<< i
                  else 0
                ) |> uint

    let epsilon = ~~~((System.UInt32.MaxValue <<< numBits) + gamma)

    int (gamma * epsilon)

let part2 (numBits, report) =
    let rec loop report i isMost =
        let numRows = Array.length report
        if numRows = 1 then report[0]
        else
            let numOnes = report |> Seq.sumBy (fun x -> getBit x i)
            let numZeros = numRows - numOnes
            let bitCriteria =
                if isMost then (if (numOnes >= numZeros) then 1 else 0)
                else (if (numZeros <= numOnes) then 0 else 1)
            let filtered = report |> Array.filter (fun x -> (getBit x i) = bitCriteria)
            loop filtered (i - 1) isMost

    let oxygen = loop report (numBits - 1) true
    let co2 = loop report (numBits - 1) false

    oxygen * co2

let input = parse "./inputs/Day03.txt"
let result1 = part1 input
let result2 = part2 input
