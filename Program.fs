#nowarn "9" "51" "20" // Don't want warnings about pointers

open System
open FSharp.NativeInterop
open System.Numerics
open System.Runtime.Intrinsics.X86
open System.Runtime.Intrinsics
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


module Array =

    let sseFloatEquals (a: array<float>) (b: array<float>) =
        let mutable result = true
        let mutable idx = 0
        let lastBlockIdx = a.Length - (a.Length % Vector128.Count)
        let aSpan = a.AsSpan ()
        let bSpan = b.AsSpan ()

        while idx < lastBlockIdx && result do
            let aVector = Vector128.Create aSpan.[idx]
            let bVector = Vector128.Create bSpan.[idx]
            let comparison = Sse2.CompareEqual (aVector, bVector)
            let matches = Sse2.MoveMask (comparison.AsByte ())

            if matches < Vector128.Count then
                result <- false

            idx <- idx + Vector128.Count

        while idx < a.Length && idx < b.Length && result do
            if a.[idx] <> b.[idx] then
                result <- false

            idx <- idx + 1

        result


    let sseIntEquals (a: array<int>) (b: array<int>) =
        let mutable result = true
        let mutable idx = 0
        let lastBlockIdx = a.Length - (a.Length % Vector128.Count)
        let aSpan = a.AsSpan ()
        let bSpan = b.AsSpan ()

        while idx < lastBlockIdx && result do
            let aVector = Vector128.Create aSpan.[idx]
            let bVector = Vector128.Create bSpan.[idx]
            let comparison = Sse2.CompareEqual (aVector, bVector)
            let matches = Sse2.MoveMask (comparison.AsByte ())

            if matches < Vector128.Count then
                result <- false

            idx <- idx + Vector128.Count

        while idx < a.Length && idx < b.Length && result do
            if a.[idx] <> b.[idx] then
                result <- false

            idx <- idx + 1

        result

    let sseByteEquals (a: Span<Byte>) (b: Span<Byte>) =
        let mutable result = true
        let mutable idx = 0
        let lastBlockIdx = a.Length - (a.Length % Vector128.Count)

        while idx < lastBlockIdx && result do
            let aVector = Vector128.Create a.[idx]
            let bVector = Vector128.Create b.[idx]
            let comparison = Sse2.CompareEqual (aVector, bVector)
            let matches = Sse2.MoveMask (comparison.AsByte ())

            if matches < Vector128.Count then
                result <- false

            idx <- idx + Vector128.Count

        while idx < a.Length && idx < b.Length && result do
            if a.[idx] <> b.[idx] then
                result <- false

            idx <- idx + 1

        result

type Settings_Default = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
}

[<CustomEquality; NoComparison>]
type Settings_SIMD_Equality = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        hash (struct (this.Capacities, this.MaxRates, this.ValveStates))

    override this.Equals b =
        match b with
        | :? Settings_SIMD_Equality as other ->
            (Array.sseFloatEquals this.Capacities other.Capacities)
            && (Array.sseFloatEquals this.MaxRates other.MaxRates)
            && (Array.sseIntEquals this.ValveStates other.ValveStates)
        | _ -> false

[<Struct; CustomEquality; NoComparison>]
type Settings_SIMD_Byte = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        hash (struct (this.Capacities, this.MaxRates, this.ValveStates))

    override this.Equals b =
        match b with
        | :? Settings_SIMD_Equality as other ->
            (Array.sseFloatEquals this.Capacities other.Capacities)
            && (Array.sseFloatEquals this.MaxRates other.MaxRates)
            && (Array.sseIntEquals this.ValveStates other.ValveStates)
        | _ -> false


[<CustomEquality; NoComparison>]
type Settings_Terrible = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        let capacityHash =
            let mutable acc = 1610612741
            let mutable idx = 0
            while idx < this.Capacities.Length do
                acc <- acc ^^^ this.Capacities.[idx].GetHashCode ()
                idx <- idx + 1

            acc

        let maxRatesHash =
            let mutable acc = 1610612741
            let mutable idx = 0
            while idx < this.MaxRates.Length do
                acc <- acc ^^^ this.MaxRates.[idx].GetHashCode ()
                idx <- idx + 1

            acc

        let valveStatesHash =
            let mutable acc = 1610612741
            let mutable idx = 0
            while idx < this.ValveStates.Length do
                acc <- acc ^^^ this.ValveStates.[idx].GetHashCode ()
                idx <- idx + 1

            acc

        capacityHash ^^^ maxRatesHash ^^^ valveStatesHash

    override this.Equals b =
        match b with
        | :? Settings_Terrible as other ->
            this.Capacities = other.Capacities
            && this.MaxRates = other.MaxRates
            && this.ValveStates = other.ValveStates
        | _ -> false


let rng = Random (123)
let maxCapacityValue = 1_000_000.0
let maxRateValue = 100_000.0
let maxValveStateValue = 10
let capacityCount = 1_000
let maxRateCount = 1_000
let valveStateCount = 1_000
let settingsCount = 1_000
let iterationCount = 1_000

// Generating our random test data
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


// We want to make sure that all of our versions of the Settings type
// have the same underlying data to hash and compare. This means we
// need to compute the indices for the underlying data and use them
// for all the versions of the Settings type we create.
let valueIndexes =
    seq {
        for _ in 1 .. settingsCount ->
        {|
            CapacityIdx = rng.Next (0, capacityCount)
            MaxRateIdx = rng.Next (0, maxRateCount)
            ValveStateIdx = rng.Next (0, valveStateCount)
        |}
    } |> Array.ofSeq


// These will be the indices for deciding which Settings values we
// will look up in each of the dictionary. We want to ensure we are
// looking up equivalent data in all the tests.
let testIndexes =
    seq {
        for _ in 1 .. iterationCount ->
            rng.Next (0, settingsCount)
    } |> Array.ofSeq


// This Settings type will use the default hash functionality built
// into .NET
let defaultSettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : Settings_Default
    } |> Array.ofSeq


// The dictionary for the Default Settings we will test lookup up values in
let defaultSettingsLookup =
    defaultSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary


// We create an array we will iterate through to lookup values in
// the Dictionary. We lay it out in an array to provide the best
// possible data locality in the loop.
let defaultSettingsTestLookups =
    seq {
        for idx in testIndexes ->
            defaultSettings.[idx]
    } |> Array.ofSeq


// Data for Settings_Terrible
let terribleSettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : Settings_Terrible
    } |> Array.ofSeq

let terribleSettingsLookup =
    terribleSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let terribleSettingsTestLookups =
    seq {
        for idx in testIndexes ->
            terribleSettings.[idx]
    } |> Array.ofSeq

// Data for Settings_SIMD_Equality
let simdEqualitySettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : Settings_SIMD_Equality
    } |> Array.ofSeq

let simdEqualitySettingsLookup =
    terribleSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let simdEqualitySettingsTestLookups =
    seq {
        for idx in testIndexes ->
            terribleSettings.[idx]
    } |> Array.ofSeq


type Benchmarks () =

    [<Benchmark>]
    member _.DefaultHash () =
        // Want a fresh RNG with a seed to ensure all versions use
        // the same lookups.
        let rng = Random (1337)
        let mutable idx = 0
        let mutable result = 0

        while idx < defaultSettingsTestLookups.Length do
            let testKey = defaultSettingsTestLookups.[idx]
            result <- defaultSettingsLookup.[testKey]

            idx <- idx + 1

        result

    [<Benchmark>]
    member _.TerribleHash () =
        // Want a fresh RNG with a seed to ensure all versions use
        // the same lookups.
        let rng = Random (1337)
        let mutable idx = 0
        let mutable result = 0

        while idx < terribleSettingsTestLookups.Length do
            let testKey = terribleSettingsTestLookups.[idx]
            result <- terribleSettingsLookup.[testKey]

            idx <- idx + 1

        result

    [<Benchmark>]
    member _.SIMD_Equality () =
        // Want a fresh RNG with a seed to ensure all versions use
        // the same lookups.
        let rng = Random (1337)
        let mutable idx = 0
        let mutable result = 0

        while idx < simdEqualitySettingsTestLookups.Length do
            let testKey = simdEqualitySettingsTestLookups.[idx]
            result <- simdEqualitySettingsLookup.[testKey]

            idx <- idx + 1

        result


[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<Benchmarks>()

    // let a = [|1.0 .. 100.0|]
    // let b = [|1.0 .. 100.0|]

    // let r1 = Array.sseFloatEquals a b
    // printfn $"Result 1: {r1}"

    // let c = [|1.0 .. 100.0|]
    // c.[0] <- 10.0

    // let r2 = Array.sseFloatEquals a c
    // printfn $"Result 2: {r2}"

    0 // return an integer exit code