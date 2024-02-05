namespace JPackages.Common.Functions

module private File =
    open System
    open System.IO

    let readFile (file : string) : string array =
        use sr = new StreamReader (file)
        let contents = sr.ReadToEnd ()
        contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)

    let writeFile (file : string) (contents : string) =
        use sw = new StreamWriter (file)
        sw.AutoFlush <- true
        contents |> sw.Write

    let writeLinesToFile (file : string) (lines : string array) =
        use sw = new StreamWriter (file)
        sw.AutoFlush <- true
        lines |> Array.iter sw.WriteLine
