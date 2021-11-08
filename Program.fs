#nowarn "9" "51" "20" // Don't want warnings about pointers

open System
open FSharp.NativeInterop
open System.Runtime.Intrinsics.X86
open System.Runtime.Intrinsics
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running




module Array =

    let inline HashCombine nr x y = (x <<< 1) + y + 631 * nr
    let private defaultHashNodes = 18

    // A derivative of the F# code for array<int32> and array<int64>
    // https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/prim-types.fs#L1277
    let floatHash (x: array<float>) =
        let len = x.Length
        let mutable idx = len - 1 
        // if i > defaultHashNodes then i <- defaultHashNodes // limit the hash
        let mutable acc = 0   
        while (idx >= 0) do 
            acc <- HashCombine idx acc (int32 x.[idx]);
            idx <- idx - 1
        acc

    let sseFloatHash (a: array<float>) =
        let len = a.Length
        let mutable idx = 0
        let mutable acc = 0

        if a.Length > 4 then
            let lastBlockIdx = a.Length - (a.Length % Vector128<float>.Count)
            let aSpan = a.AsSpan ()
            let aPointer = && (aSpan.GetPinnableReference ())
            let aVector = Sse2.LoadVector128 (NativePtr.add aPointer idx)
            let accVector = Vector128.Create 0.0

            idx <- idx + Vector128.Count

        while idx < a.Length do
            acc <- HashCombine idx acc (int32 a.[idx]);
            idx <- idx - 1

        acc



    let sseFloatEquals (a: array<float>) (b: array<float>) =
        if a.Length = b.Length then
            let mutable result = true
            let mutable idx = 0
            
            if a.Length > 4 then
                let lastBlockIdx = a.Length - (a.Length % Vector128<float>.Count)
                let aSpan = a.AsSpan ()
                let bSpan = b.AsSpan ()
                let aPointer = && (aSpan.GetPinnableReference ())
                let bPointer = && (bSpan.GetPinnableReference ())

                while idx < lastBlockIdx && result do
                    let aVector = Sse2.LoadVector128 (NativePtr.add aPointer idx)
                    let bVector = Sse2.LoadVector128 (NativePtr.add bPointer idx)
                    let comparison = Sse2.CompareEqual (aVector, bVector)
                    let mask = Sse2.MoveMask (comparison.AsDouble())
                    if mask <> 0x3 then
                        result <- false

                    idx <- idx + Vector128.Count

            while idx < a.Length && idx < b.Length && result do
                if a.[idx] <> b.[idx] then
                    result <- false

                idx <- idx + 1

            result

        else
            false

    let avxFloatEquals (a: array<float>) (b: array<float>) =
        if a.Length = b.Length then
            let mutable result = true
            let mutable idx = 0
            
            if a.Length > 8 then
                let lastBlockIdx = a.Length - (a.Length % Vector256.Count)
                let aSpan = a.AsSpan ()
                let bSpan = b.AsSpan ()
                let aPointer = && (aSpan.GetPinnableReference ())
                let bPointer = && (bSpan.GetPinnableReference ())
                let zeroVector = Vector256.Create 0.0
                while idx < lastBlockIdx && result do
                    let aVector = Avx2.LoadVector256 (NativePtr.add aPointer idx)
                    let bVector = Avx2.LoadVector256 (NativePtr.add bPointer idx)
                    let comparison = Avx2.CompareEqual (aVector, bVector)
                    let mask = Avx2.MoveMask (comparison.AsDouble())
                    if mask <> 0xF then
                        result <- false

                    idx <- idx + Vector256.Count

            while idx < a.Length && idx < b.Length && result do
                if a.[idx] <> b.[idx] then
                    result <- false

                idx <- idx + 1

            result

        else
            false


    let sseIntEquals (a: array<int>) (b: array<int>) =
        if a.Length = b.Length then
            let mutable result = true
            let mutable idx = 0
            
            if a.Length > 4 then
                let lastBlockIdx = a.Length - (a.Length % Vector128.Count)
                let aSpan = a.AsSpan ()
                let bSpan = b.AsSpan ()
                let aPointer = && (aSpan.GetPinnableReference ())
                let bPointer = && (bSpan.GetPinnableReference ())
                let zeroVector = Vector128.Create 0

                while idx < lastBlockIdx && result do
                    let aVector = Sse2.LoadVector128 (NativePtr.add aPointer idx)
                    let bVector = Sse2.LoadVector128 (NativePtr.add bPointer idx)
                    let comparison = Sse2.CompareEqual (aVector, bVector)
                    let mask = Sse2.MoveMask (comparison.AsDouble())
                    if mask <> 0x3 then
                        result <- false
                            
                    idx <- idx + Vector128.Count

            while idx < a.Length && idx < b.Length && result do
                if a.[idx] <> b.[idx] then
                    result <- false

                idx <- idx + 1

            result

        else
            false

    let avxIntEquals (a: array<int>) (b: array<int>) =
        if a.Length = b.Length then
            let mutable result = true
            let mutable idx = 0

            if a.Length > 8 then
                let lastBlockIdx = a.Length - (a.Length % Vector256.Count)
                let aSpan = a.AsSpan ()
                let bSpan = b.AsSpan ()
                let aPointer = && (aSpan.GetPinnableReference ())
                let bPointer = && (bSpan.GetPinnableReference ())
                let zeroVector = Vector256.Create 0
                
                while idx < lastBlockIdx && result do
                    let aVector = Avx2.LoadVector256 (NativePtr.add aPointer idx)
                    let bVector = Avx2.LoadVector256 (NativePtr.add bPointer idx)
                    let comparison = Avx2.CompareEqual (aVector, bVector)
                    let matches = Avx2.MoveMask (comparison.AsDouble())
                    if matches <> 0xF then
                        result <- false

                    idx <- idx + Vector256.Count

            // Cleanup loop
            while idx < a.Length && idx < b.Length && result do
                if a.[idx] <> b.[idx] then
                    result <- false

                idx <- idx + 1

            result

        else
            false


[<Struct>]
type Settings = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
}


type SimpleComparer () =
    interface IEqualityComparer<Settings> with
        member _.Equals (a, b) =
            a.Capacities = b.Capacities
            && a.MaxRates = b.MaxRates
            && a.ValveStates = b.ValveStates
            
        member _.GetHashCode (a) =
            let capacitiesHash = Array.floatHash a.Capacities
            let maxRatesHash = Array.floatHash a.MaxRates
            // Using a different method because F# has speciliazed code for array<int>
            let valveStatesHash = a.ValveStates.GetHashCode ()

            hash (struct (capacitiesHash, maxRatesHash, valveStatesHash))


type SseComparer () =
    interface IEqualityComparer<Settings> with
        member _.Equals (a, b) =
            (Array.sseFloatEquals a.Capacities b.Capacities)
            && (Array.sseFloatEquals a.MaxRates b.MaxRates)
            && (Array.sseIntEquals a.ValveStates b.ValveStates)

        member _.GetHashCode (a) =
            let capacitiesHash = Array.floatHash a.Capacities
            let maxRatesHash = Array.floatHash a.MaxRates
            // Using a different method because F# has speciliazed code for array<int>
            let valveStatesHash = a.ValveStates.GetHashCode ()

            capacitiesHash ^^^ maxRatesHash ^^^ valveStatesHash


type AvxComparer () =
    interface IEqualityComparer<Settings> with
        member _.Equals (a, b) =
            (Array.avxFloatEquals a.Capacities b.Capacities)
            && (Array.avxFloatEquals a.MaxRates b.MaxRates)
            && (Array.avxIntEquals a.ValveStates b.ValveStates)

        member _.GetHashCode (a) =
            let capacitiesHash = Array.floatHash a.Capacities
            let maxRatesHash = Array.floatHash a.MaxRates
            // Using a different method because F# has speciliazed code for array<int>
            let valveStatesHash = a.ValveStates.GetHashCode ()

            capacitiesHash ^^^ maxRatesHash ^^^ valveStatesHash


[<Struct; CustomEquality; NoComparison>]
type SettingsSimpleOverride = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        let capacitiesHash = Array.floatHash this.Capacities
        let maxRatesHash = Array.floatHash this.MaxRates
        // Using a different method because F# has speciliazed code for array<int>
        let valveStatesHash = this.ValveStates.GetHashCode ()

        hash (struct (capacitiesHash, maxRatesHash, valveStatesHash))

    override this.Equals b =
        match b with
        | :? SettingsSimpleOverride as other ->
            this.Capacities = other.Capacities
            && this.MaxRates = other.MaxRates
            && this.ValveStates = other.ValveStates
        | _ -> false


[<Struct; CustomEquality; NoComparison>]
type SettingsJeorg = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        let bytes = Array.concat [|
            this.Capacities |> Array.map (fun (f:float) -> BitConverter.GetBytes(f)) |> Array.concat
            this.MaxRates |> Array.map (fun (f:float) -> BitConverter.GetBytes(f)) |> Array.concat
            this.ValveStates |> Array.map (fun (i:int) -> BitConverter.GetBytes(i)) |> Array.concat
        |]
        let sha256 = Security.Cryptography.SHA256.Create().ComputeHash(bytes)
        // only the first 4 bytes are used but the SHA-2 has the property that any sub-range is also 
        // usable but with increased chance of collision of course.
        BitConverter.ToInt32(sha256, 0) 

    override this.Equals b =
        match b with
        | :? SettingsJeorg as other ->
            (Array.sseFloatEquals this.Capacities other.Capacities)
            && (Array.sseFloatEquals this.MaxRates other.MaxRates)
            && (Array.sseIntEquals this.ValveStates other.ValveStates)
        | _ -> false


[<Struct; CustomEquality; NoComparison>]
type SettingsSseOverride = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        let capacitiesHash = Array.floatHash this.Capacities
        let maxRatesHash = Array.floatHash this.MaxRates
        // Using a different method because F# has speciliazed code for array<int>
        let valveStatesHash = this.ValveStates.GetHashCode ()

        // hash (struct (capacitiesHash, maxRatesHash, valveStatesHash))
        capacitiesHash ^^^ maxRatesHash ^^^ valveStatesHash

    override this.Equals b =
        match b with
        | :? SettingsSseOverride as other ->
            (Array.sseFloatEquals this.Capacities other.Capacities)
            && (Array.sseFloatEquals this.MaxRates other.MaxRates)
            && (Array.sseIntEquals this.ValveStates other.ValveStates)
        | _ -> false

[<Struct; CustomEquality; NoComparison>]
type SettingsAvxOverride = {
    Capacities : array<float>
    MaxRates : array<float>
    ValveStates : array<int>
} with
    override this.GetHashCode () =
        let capacitiesHash = Array.floatHash this.Capacities
        let maxRatesHash = Array.floatHash this.MaxRates
        // Using a different method because F# has speciliazed code for array<int>
        let valveStatesHash = this.ValveStates.GetHashCode ()

        // hash (struct (capacitiesHash, maxRatesHash, valveStatesHash))
        capacitiesHash ^^^ maxRatesHash ^^^ valveStatesHash

    override this.Equals b =
        match b with
        | :? SettingsAvxOverride as other ->
            (Array.avxFloatEquals this.Capacities other.Capacities)
            && (Array.avxFloatEquals this.MaxRates other.MaxRates)
            && (Array.avxIntEquals this.ValveStates other.ValveStates)
        | _ -> false


// Setting up test data
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
let settings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : Settings
    } |> Array.ofSeq

// Data for Settings_Terrible
let simpleOverrideSettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : SettingsSimpleOverride
    } |> Array.ofSeq

// Data for Settings_SIMD_Equality
let sseOverrideSettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : SettingsSseOverride
    } |> Array.ofSeq

// Data for Settings_SIMD_Equality
let avxOverrideSettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : SettingsAvxOverride
    } |> Array.ofSeq

let jeorgSettings =
    seq {
        for vi in valueIndexes ->
        {
            Capacities = capacities.[vi.CapacityIdx]
            MaxRates = maxRates.[vi.MaxRateIdx]
            ValveStates = valveStates.[vi.ValveStateIdx]
        } : SettingsJeorg
    } |> Array.ofSeq

// Create the Dictionaries we will be looking up data in
// The dictionary for the Default Settings we will test lookup up values in
let settingsDictionary =
    settings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let simpleComparerDictionary =
    let values = 
        settings
        |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    let comparer = SimpleComparer ()
    Dictionary (values, comparer)

let sseComparerDictionary =
    let values = 
        settings
        |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    let comparer = SseComparer ()
    Dictionary (values, comparer)

let avxComparerDictionary =
    let values = 
        settings
        |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    let comparer = AvxComparer ()
    Dictionary (values, comparer)

let simpleOverrideDictionary =
    simpleOverrideSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let sseOverrideDictionary =
    sseOverrideSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let avxOverrideDictionary =
    avxOverrideSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary

let jeorgSettingsDictionary =
    jeorgSettings
    |> Seq.mapi (fun idx setting -> KeyValuePair (setting, idx))
    |> Dictionary


// We create an array we will iterate through to lookup values in
// the Dictionary. We lay it out in an array to provide the best
// possible data locality in the loop.
let settingsKeys =
    seq {
        for idx in testIndexes ->
            settings.[idx]
    } |> Array.ofSeq


let simpleOverrideKeys =
    seq {
        for idx in testIndexes ->
            simpleOverrideSettings.[idx]
    } |> Array.ofSeq


let sseOverrideKeys =
    seq {
        for idx in testIndexes ->
            sseOverrideSettings.[idx]
    } |> Array.ofSeq


let avxOverrideKeys =
    seq {
        for idx in testIndexes ->
            avxOverrideSettings.[idx]
    } |> Array.ofSeq


let jeorgKeys =
    seq {
        for idx in testIndexes ->
            jeorgSettings.[idx]
    } |> Array.ofSeq



type Benchmarks () =

    [<Benchmark>]
    member _.Default () =
        let mutable idx = 0
        let mutable result = 0

        while idx < settingsKeys.Length do
            let testKey = settingsKeys.[idx]
            result <- settingsDictionary.[testKey]

            idx <- idx + 1

        result

    [<Benchmark>]
    member _.Jeorg () =
        let mutable idx = 0
        let mutable result = 0

        while idx < jeorgKeys.Length do
            let testKey = jeorgKeys.[idx]
            result <- jeorgSettingsDictionary.[testKey]

            idx <- idx + 1

        result


    [<Benchmark>]
    member _.SimpleComparer () =
        let mutable idx = 0
        let mutable result = 0

        while idx < settingsKeys.Length do
            let testKey = settingsKeys.[idx]
            result <- simpleComparerDictionary.[testKey]

            idx <- idx + 1

        result



    [<Benchmark>]
    member _.SimpleOverride () =
        let mutable idx = 0
        let mutable result = 0

        while idx < simpleOverrideKeys.Length do
            let testKey = simpleOverrideKeys.[idx]
            result <- simpleOverrideDictionary.[testKey]

            idx <- idx + 1

        result


    [<Benchmark>]
    member _.SseOverride () =
        let mutable idx = 0
        let mutable result = 0

        while idx < sseOverrideKeys.Length do
            let testKey = sseOverrideKeys.[idx]
            result <- sseOverrideDictionary.[testKey]

            idx <- idx + 1

        result


    [<Benchmark>]
    member _.SseComparer () =
        let mutable idx = 0
        let mutable result = 0

        while idx < settingsKeys.Length do
            let testKey = settingsKeys.[idx]
            result <- sseComparerDictionary.[testKey]

            idx <- idx + 1

        result


    [<Benchmark>]
    member _.AvxComparer () =
        let mutable idx = 0
        let mutable result = 0

        while idx < settingsKeys.Length do
            let testKey = settingsKeys.[idx]
            result <- avxComparerDictionary.[testKey]

            idx <- idx + 1

        result


    [<Benchmark>]
    member _.AvxOverride () =
        let mutable idx = 0
        let mutable result = 0

        while idx < avxOverrideKeys.Length do
            let testKey = avxOverrideKeys.[idx]
            result <- avxOverrideDictionary.[testKey]

            idx <- idx + 1

        result


let profileCustom () =
        let mutable idx = 0
        let mutable result = 0
        let iterations = 10_000_000
        let rng = Random 42

        while idx < iterations do
            let testKey = simpleOverrideKeys.[rng.Next (0, simpleOverrideKeys.Length)]
            result <- simpleOverrideDictionary.[testKey]
            idx <- idx + 1

        result

let profileSse () =
        let mutable idx = 0
        let mutable result = 0
        let iterations = 10_000_000
        let rng = Random 42

        while idx < iterations do
            let testKey = sseOverrideKeys.[rng.Next (0, sseOverrideKeys.Length)]
            result <- sseOverrideDictionary.[testKey]
            idx <- idx + 1

        result

let profileAvx () =
        let mutable idx = 0
        let mutable result = 0
        let iterations = 10_000_000
        let rng = Random 42

        while idx < iterations do
            let testKey = avxOverrideKeys.[rng.Next (0, avxOverrideKeys.Length)]
            result <- avxOverrideDictionary.[testKey]
            idx <- idx + 1

        result


[<EntryPoint>]
let main argv =

    match argv.[0].ToLower() with
    | "benchmark" ->
        let summary = BenchmarkRunner.Run<Benchmarks>()
        ()
    | "profilecustom" ->
        let result = profileCustom ()
        printfn "%A" result
    | "profilesse" ->
        let result = profileSse ()
        printfn "%A" result
    | _ ->
        printfn $"Unknown command: {argv.[0]}"

//    profileSse ()
    
//    profileAvx ()

//    let a = [|1.0 .. 4.0|]
//    let b = [|1.0 .. 4.0|]
//    let aSpan = a.AsSpan ()
//    let bSpan = b.AsSpan ()
//    let aPointer = && (aSpan.GetPinnableReference ())
//    let bPointer = && (bSpan.GetPinnableReference ())
//    let aVector = Sse2.LoadVector128 (NativePtr.add aPointer 0)
//    let bVector = Sse2.LoadVector128 (NativePtr.add bPointer 0)
//    let comparison = Sse2.CompareEqual (aVector, bVector)
//    let mask = Sse2.MoveMask (comparison.AsDouble())
//    let sse2Result = (mask = 0x3)
//    printfn $"Should be true: {sse2Result}"
//    
//    let a = [|1 .. 8|]
//    let b = [|1 .. 8|]
//    let aSpan = a.AsSpan ()
//    let bSpan = b.AsSpan ()
//    let aPointer = && (aSpan.GetPinnableReference ())
//    let bPointer = && (bSpan.GetPinnableReference ())
//    let aVector = Avx2.LoadVector256 (NativePtr.add aPointer 0)
//    let bVector = Avx2.LoadVector256 (NativePtr.add bPointer 0)
//    let comparison = Avx2.CompareEqual (aVector, bVector)
//    let mask = Avx2.MoveMask (comparison.AsDouble())
//    let test = (mask = 0xF)
//    printfn $"Should be true: {test}"
//    
    0 // return an integer exit code