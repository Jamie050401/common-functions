module JPackages.Common.BenchmarkRunner

open JPackages.Common.Benchmarks
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<Collections> () |> ignore
    0
