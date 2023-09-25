namespace CommonFunctions

type ErrorType =
    | Unhandled
    | IO
    | Value

// Error Codes:
//   0 - Default
//   1 - Unhandled
//   2 - File already exists
//   3 - File does not exist
//   4 - File is empty
//   5 - Not all values are unique
type Error =
    { Code : int
      Type : ErrorType
      Message : string
      InnerException : exn option }

    static member Default = { Code = 0; Type = Unhandled; Message = ""; InnerException = None }

[<RequireQualifiedAccess>]
type Response =
    | Success
    | Failure of Error

    static member Unhandled ex = Failure { Code = 1; Type = Unhandled; Message = "Unhandled exception"; InnerException = Some ex }

[<RequireQualifiedAccess>]
type ResponseWithValue<'a> =
    | Success of 'a
    | Failure of Error

    static member Unhandled ex = ResponseWithValue.Failure { Code = 1; Type = Unhandled; Message = "Unhandled exception"; InnerException = Some ex }

module String =
    open System

    let split (chars : char array) (options : StringSplitOptions) (str : string) =
        str.Split (chars, options)

module Maths =
    open System

    let round (number : float) (decimalPlaces : int) = Math.Round (number + 0.00000001, decimalPlaces)
    let round2dp (number : float) = round number 2

module TuplesOfThree =
    let fst (tuple : 'a * 'b * 'c) = match tuple with | value, _, _ -> value
    let snd (tuple : 'a * 'b * 'c) = match tuple with | _, value, _ -> value
    let thrd (tuple : 'a * 'b * 'c) = match tuple with | _, _, value -> value

module TuplesOfFour =
    let fst (tuple : 'a * 'b * 'c * 'd) = match tuple with | value, _, _, _ -> value
    let snd (tuple : 'a * 'b * 'c * 'd) = match tuple with | _, value, _, _ -> value
    let thrd (tuple : 'a *'b * 'c * 'd) = match tuple with | _, _, value, _ -> value
    let frth (tuple : 'a *'b * 'c * 'd) = match tuple with | _, _, _, value -> value

module private FileOperations =
    open System
    open System.IO

    let readFile (file : string) =
        use sr = new StreamReader (file)
        let contents = sr.ReadToEnd ()
        contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)

    let readFileAsync (file : string) =
        async {
            use sr = new StreamReader (file)
            let! contents = sr.ReadToEndAsync () |> Async.AwaitTask
            return contents.Split ([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
        }
        |> Async.RunSynchronously

    let writeFile (file : string) (lines : string array) =
        use sw = new StreamWriter (file)
        lines |> Array.iter (sw.WriteLine)

    let writeFileAsync (file : string) (lines : string array) =
        async {
            use sw = new StreamWriter (file)
            for line in lines do
                return! line |> sw.WriteLineAsync |> Async.AwaitTask
        }
        |> Async.RunSynchronously

module Csv =
    open System
    open System.IO

    type File =
        | WithHeaders of Map<string, string>
        | WithoutHeaders of string array array

    // TODO - Enhance this function to account for escaped commas
    let private split (str : string) =
        str |> String.split [| ',' |] StringSplitOptions.RemoveEmptyEntries

    let createFile filePath fileName overwrite =
        try
            let file = $"{filePath}\\{fileName}.csv"
            Directory.CreateDirectory filePath |> ignore
            match File.Exists file with
            | true ->
                match overwrite with
                | true -> 
                    File.Delete file
                    File.Create file |> ignore
                    Response.Success
                | false ->
                    let msg = $"Failed to create file '{file}' since it already exists"
                    Response.Failure { Code = 2; Type = IO; Message = msg; InnerException = None }
            | false ->
                File.Create file |> ignore
                Response.Success
        with
        | ex ->
            Response.Unhandled ex

    let deleteFile filePath fileName =
        try
            let file = $"{filePath}\\{fileName}.csv"
            match File.Exists file with
            | true ->
                File.Delete file
                Response.Success
            | false ->
                let msg = $"Failed to delete file '{file}' since it does not exist"
                Response.Failure { Code = 3; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            Response.Unhandled ex

    let readFile filePath fileName async headers =
        let read file =
            match async with
            | true  -> FileOperations.readFileAsync file
            | false -> FileOperations.readFile file

        try
            let file = $"{filePath}\\{fileName}.csv"
            match File.Exists file with
            | true ->
                let contents = read file
                match contents |> Array.isEmpty with
                | true ->
                    let msg = $"Failed to read file '{file}' since it is empty"
                    ResponseWithValue.Failure { Code = 4; Type = IO; Message = msg; InnerException = None }
                | false ->
                    match headers with
                    | true ->
                        let headers = contents |> Array.head |> split
                        match (headers |> Seq.distinct |> Seq.length) = (headers |> Array.length) with
                        | true ->
                            contents |> Array.tail
                            |> Array.Parallel.mapi (fun index content ->
                                headers.[index], content)
                            |> Map.ofArray
                            |> WithHeaders
                            |> ResponseWithValue.Success
                        | false ->
                            let msg = $"Failed to read file '{file}' since not all column headings are unique"
                            ResponseWithValue.Failure { Code = 5; Type = Value; Message = msg; InnerException = None }
                    | false ->
                        contents |> Array.Parallel.map (split)
                        |> WithoutHeaders
                        |> ResponseWithValue.Success
            | false ->
                let msg = $"Failed to read file '{file}' since it does not exist"
                ResponseWithValue.Failure { Code = 3; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            ResponseWithValue<_>.Unhandled ex

    let writeFile contents filePath fileName async overwrite =
        let getLines () =
            match contents with
            | WithHeaders contents ->
                raise (System.NotImplementedException ())
            | WithoutHeaders contents ->
                raise (System.NotImplementedException ())

        let write file lines =
            match async with
            | true  -> FileOperations.writeFileAsync file lines
            | false -> FileOperations.writeFile file lines

        try
            let file = $"{filePath}\\{fileName}.csv"
            match File.Exists file with
            | true ->
                match overwrite with
                | true ->
                    File.Delete file
                    write file (getLines ())
                    Response.Success
                | false ->
                    let msg = $"Failed to write file '{file}' since it already exists"
                    Response.Failure { Code = 2; Type = IO; Message = msg; InnerException = None }
            | false ->
                write file (getLines ())
                Response.Success
        with
        | ex ->
            Response.Unhandled ex
