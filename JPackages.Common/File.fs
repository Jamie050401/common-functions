namespace JPackages.Common.Functions

module private File =
    open System
    open System.IO

    let readFile (file : string) : string array =
        use sr = new StreamReader (file)
        let contents = sr.ReadToEnd ()
        contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)

    //let readFileAsync (file : string) : string array =
    //    use sr = new StreamReader (file)
    //    
    //    let readAsync () =
    //        async {
    //            return! sr.ReadToEndAsync () |> Async.AwaitTask
    //        }
    //    
    //    readAsync ()
    //    |> Async.RunSynchronously
    //    |> fun contents ->
    //        contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)

    let write (file : string) (contents : string) =
        use sw = new StreamWriter (file)
        contents |> sw.Write
    
    let writeFile (file : string) (lines : string array) =
        use sw = new StreamWriter (file)
        lines |> Array.iter sw.WriteLine

    //let writeFileAsync (file : string) (lines : string array) =
    //    use sw = new StreamWriter (file)
    //    
    //    let writeAsync (line : string) =
    //        async {
    //            return! line |> sw.WriteLineAsync |> Async.AwaitTask   
    //        }
    //    
    //    lines
    //    |> Array.map writeAsync
    //    |> Async.Sequential
    //    |> Async.RunSynchronously
    //    |> ignore
