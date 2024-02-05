namespace JPackages.Common.Benchmarks

open BenchmarkDotNet.Attributes

[<MemoryDiagnoser>]
type Collections () =
    let list = List.init 1000 id
    let list2 = List.init 1000 (fun index -> 1000 + index)
    let list3 = List.init 1000 (fun index -> 2000 + index)
    let arr = Array.init 1000 id
    let arr2 = Array.init 1000 (fun index -> 1000 + index)
    let arr3 = Array.init 1000 (fun index -> 2000 + index)

    [<Benchmark>]
    member this.ListMap () =
        list |> List.map (fun value -> float value + 1.0)

    [<Benchmark>]
    member this.ArrayMap () =
        arr |> Array.map (fun value -> float value + 1.0)

    [<Benchmark>]
    member this.ListAppend () = List.append list list2

    [<Benchmark>]
    member this.ArrayAppend () = Array.append arr arr2

    [<Benchmark>]
    member this.ListPrepend () = 1001 :: list

    [<Benchmark>]
    member this.ArrayPrepend () = Array.append [| 1001 |] arr

    [<Benchmark>]
    member this.ListConcat () = List.concat [ list; list2; list3 ]

    [<Benchmark>]
    member this.ArrayConcat () = Array.concat [| arr; arr2; arr3 |]

    [<Benchmark>]
    member this.ListFind () =
        list |> List.find (fun elem -> elem = 99) |> ignore
        list |> List.find (fun elem -> elem = 999)

    [<Benchmark>]
    member this.ArrayFind () =
        arr |> Array.find (fun elem -> elem = 99) |> ignore
        arr |> Array.find (fun elem -> elem = 999)
