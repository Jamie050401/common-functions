namespace Common.Functions

open Common.Domain

module Tuples =
    module TuplesOfThree =
        let first tuple = match tuple with | value, _, _ -> value
        let second tuple = match tuple with | _, value, _ -> value
        let third tuple = match tuple with | _, _, value -> value

    module TuplesOfFour =
        let first tuple = match tuple with | value, _, _, _ -> value
        let second tuple = match tuple with | _, value, _, _ -> value
        let third tuple = match tuple with | _, _, value, _ -> value
        let fourth tuple = match tuple with | _, _, _, value -> value

    module TuplesOfFive =
        let first tuple = match tuple with | value, _, _, _, _ -> value
        let second tuple = match tuple with | _, value, _, _, _ -> value
        let third tuple = match tuple with | _, _, value, _, _ -> value
        let fourth tuple = match tuple with | _, _, _, value, _ -> value
        let fifth tuple = match tuple with | _, _, _, _, value -> value

open Tuples

module String =
    open System

    let split (options : StringSplitOptions) (chars : char array) (str : string) =
        str.Split (chars, options)

module Math =
    open System

    let private rnd (``type`` : RoundingType) (precision : int) (number : float) =
        let round number (precision : int) =
            (floor ((abs number + 0.5 / 10.0 ** precision) * 10.0 ** precision)) / 10.0 ** precision
            
        let truncate number (precision : int) =
            (floor (abs number * 10.0 ** precision)) / 10.0 ** precision
        
        let isRoundingReqd number (precision : int) =
            abs number * 10.0 ** precision
            |> fun value -> round (value - truncate value 0) 1 <> 0.0
        
        let roundUp number (precision : int) =
            let isRoundingReqd = isRoundingReqd number precision
            match isRoundingReqd with
            | true  -> (ceil (abs number * 10.0 ** precision)) / 10.0 ** precision
            | false -> number
            
        let roundDown number (precision : int) =
            let isRoundingReqd = isRoundingReqd number precision
            match isRoundingReqd with
            | true  -> (floor (abs number * 10.0 ** precision)) / 10.0 ** precision
            | false -> number
        
        match ``type`` with
        | RoundingType.Decimal behaviour ->
            match behaviour with
            | RoundingBehaviour.Standard -> round number precision
            | RoundingBehaviour.Truncate -> truncate number precision
            | RoundingBehaviour.Up       -> roundUp number precision
            | RoundingBehaviour.Down     -> roundDown number precision
        | RoundingType.Significant behaviour ->
            behaviour |> ignore
            raise (NotImplementedException ())
        |> fun rounded ->
            match number < 0.0 with
            | true  -> rounded * -1.0
            | false -> rounded
            
    let round (precision : int) (number : float) =
        rnd (RoundingType.Decimal RoundingBehaviour.Standard) precision number
        
    let truncate (precision : int) (number : float) =
        rnd (RoundingType.Decimal RoundingBehaviour.Truncate) precision number

module private FileOperations =
    open System
    open System.IO

    let readFile (file : string) : string array =
        use sr = new StreamReader (file)
        let contents = sr.ReadToEnd ()
        contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)

    let readFileAsync (file : string) : string array =
        use sr = new StreamReader (file)
        
        let readAsync () =
            async {
                return! sr.ReadToEndAsync () |> Async.AwaitTask
            }
        
        readAsync ()
        |> Async.RunSynchronously
        |> fun contents ->
            contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)

    let writeFile (file : string) (lines : string array) =
        use sw = new StreamWriter (file)
        lines |> Array.iter sw.WriteLine

    let writeFileAsync (file : string) (lines : string array) =
        use sw = new StreamWriter (file)
        
        let writeAsync (line : string) =
            async {
                return! line |> sw.WriteLineAsync |> Async.AwaitTask   
            }
        
        lines
        |> Array.map writeAsync
        |> Async.Sequential
        |> Async.RunSynchronously
        |> ignore

module Csv =
    open System
    open System.IO

    let private split (str : string) =
        str.ToCharArray ()
        |> ((false, "", Array.empty) |> Array.fold (fun (isEscaped, str, strs) char ->
            match char |> Char.IsWhiteSpace with
            | true ->
                isEscaped, str, strs
            | false ->
                match char = '"' with
                | true ->
                    not isEscaped, str + "\"", strs
                | false ->
                    match isEscaped with
                    | true ->
                        isEscaped, str + string char, strs
                    | false ->
                        match char <> ',' with
                        | true  -> isEscaped, str + string char, strs
                        | false -> isEscaped, "", Array.append strs [| str |]))
        |> TuplesOfThree.third

    let createFile file overwrite =
        let filePath = (Directory.GetParent file).FullName
        Directory.CreateDirectory filePath |> ignore
        match File.Exists file with
        | true ->
            match overwrite with
            | true -> 
                File.Delete file
                File.Create file |> ignore
            | false ->
                ()
        | false ->
            File.Create file |> ignore

    let tryCreateFile (file : string) overwrite =
        try
            match file.EndsWith ".csv" with
            | true ->
                match File.Exists file && overwrite with
                | true ->
                    createFile file overwrite
                    Response.Success
                | false ->
                    let msg = "Failed to create file" + file + "since it already exists"
                    Response.Failure { Code = 2; Type = IO; Message = msg; InnerException = None }
            | false ->
                let msg = "Invalid file type supplied, only .csv files are supported by this function"
                Response.Failure { Code = 6; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            Response.Unhandled ex

    let deleteFile file =
        match File.Exists file with
        | true  -> File.Delete file
        | false -> ()

    let tryDeleteFile file =
        try
            match File.Exists file with
            | true ->
                deleteFile file
                Response.Success
            | false ->
                let msg = "Failed to delete file" + file + "since it does not exist"
                Response.Failure { Code = 3; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            Response.Unhandled ex

    let readFile file useAsync =
        let read file =
            match useAsync with
            | true  -> FileOperations.readFileAsync file
            | false -> FileOperations.readFile file

        match File.Exists file with
        | true ->
            let contents = read file
            contents
            |> Array.Parallel.map split
        | false ->
            Array.empty

    let tryReadFile file useAsync =
        try
            match File.Exists file with
            | true ->
                let contents = readFile file useAsync
                ResponseWithValue.Success contents
            | false ->
                let msg = "Failed to read file" + file + "since it does not exist"
                ResponseWithValue.Failure { Code = 3; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            ResponseWithValue<_>.Unhandled ex

    let private getLines (contents : 'a array array) =
        contents |> Array.map (fun columns ->
            columns |> ("" |> Array.fold (fun str column ->
                match str |> String.IsNullOrEmpty with
                | true  -> column.ToString ()
                | false -> str + "," + column.ToString ())))
    
    let private write file lines useAsync =
        match useAsync with
        | true  -> FileOperations.writeFileAsync file lines
        | false -> FileOperations.writeFile file lines
    
    let writeFile contents file useAsync overwrite =
        match File.Exists file && overwrite with
        | true  -> File.Delete file
        | false -> ()

        match File.Exists file with
        | true  -> ()
        | false -> write file (getLines contents) useAsync
        
    let tryWriteFile contents file useAsync overwrite =
        try
            match File.Exists file && overwrite with
            | true  -> File.Delete file
            | false -> ()
            
            match File.Exists file with
            | true  ->
                let msg = "Failed to write file" + file + "since it already exists"
                Response.Failure { Code = 2; Type = IO; Message = msg; InnerException = None }
            | false ->
                write file (getLines contents) useAsync
                Response.Success
        with
        | ex ->
            Response.Unhandled ex
