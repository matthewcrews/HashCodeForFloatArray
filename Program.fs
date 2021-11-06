open System
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


type Settings_Default = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
}



let rng = System.Random (123)
let maxCapacityValue = 1_000_000.0
let maxRateValue = 100_000.0
let maxValveStateValue = 10
let capacityCount = 1_000
let maxRateCount = 1_000
let valveStateCount = 1_000
let settingsCount = 1_000
let iterationCount = 1_000


let randomFloatArray (rng: Random) maxValue length =
    let result = Array.zeroCreate length

    for i = 0 to result.Length - 1 do
        result.[i] <- maxValue * (rng.NextDouble ())

    result

let randomIntArray (rng: Random) maxValue length =
    let result = Array.zeroCreate length

    for i = 0 to result.Length - 1 do
        result.[i] <- rng.Next (0, maxValue)

    result

let capacities =
    seq {
        for _ in 1 .. capacityCount ->
            rng.Next (1, 100)
            |> randomFloatArray rng maxCapacityValue
    } |> Array.ofSeq

let maxRates =
    seq {
        for _ in 1 .. maxRateCount ->
            rng.Next (1, 100)
            |> randomFloatArray rng maxRateValue
    } |> Array.ofSeq

let valveStates =
    seq {
        for _ in 1 .. valveStateCount ->
            rng.Next (1, 100)
            |> randomIntArray rng maxValveStateValue
    } |> Array.ofSeq

let settings =
    seq {
        for _ in 1 .. settingsCount ->
        {
            Capacities = capacities.[rng.Next (0, capacityCount)]
            MaxRates = maxRates.[rng.Next (0, maxRateCount)]
            ValveStates = valveStates.[rng.Next (0, valveStateCount)]
        }
    } |> Array.ofSeq

let settingsLookup =
    settings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let testLookups =
    seq {
        for _ in 1 .. iterationCount ->
            settings.[rng.Next (0, settings.Length - 1)]
    } |> Array.ofSeq


type Benchmarks () =

    [<Benchmark>]
    member _.DefaultHash () =
        // Want a fresh RNG with a seed to ensure all versions use
        // the same lookups.
        let rng = Random (1337)
        let mutable idx = 0
        let mutable result = 0

        while idx < testLookups.Length do
            let testKey = testLookups.[idx]
            result <- settingsLookup.[testKey]

            idx <- idx + 1

        result


[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<Benchmarks>()

    0 // return an integer exit code