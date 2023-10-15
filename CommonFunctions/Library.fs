namespace CommonFunctions

type ErrorType =
    | Unhandled
    | IO
    | Value
    | Argument

// Error Codes:
//   0 - Default
//   1 - Unhandled
//   2 - File already exists
//   3 - File does not exist
//   4 - File is empty
//   5 - Not all values are unique
//   6 - Invalid file type
type Error =
    { Code : int
      Type : ErrorType
      Message : string
      InnerException : exn option }

    static member Default = { Code = 0; Type = Unhandled; Message = ""; InnerException = None }
    static member Unhandled ex = { Code = 1; Type = Unhandled; Message = "Unhandled exception"; InnerException = Some ex }

[<RequireQualifiedAccess>]
type Response =
    | Success
    | Failure of Error

    static member Unhandled ex = Failure (Error.Unhandled ex)

[<RequireQualifiedAccess>]
type ResponseWithValue<'a> =
    | Success of 'a
    | Failure of Error

    static member Unhandled ex = Failure (Error.Unhandled ex)

module String =
    open System

    let split (chars : char array) (options : StringSplitOptions) (str : string) =
        str.Split (chars, options)

module Maths =
    open System

    let round (number : float) (decimalPlaces : int) = Math.Round (number + 0.00000001, decimalPlaces)
    let round2dp (number : float) = round number 2

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
        lines |> Array.iter sw.WriteLine

    let writeFileAsync (file : string) (lines : string array) =
        async {
            use sw = new StreamWriter (file)
            for line in lines do
                return! line |> sw.WriteLineAsync |> Async.AwaitTask
        }
        |> Async.RunSynchronously

// TODO - Need to finish implementing 'writeFile' function
// TODO - Need to implement 'tryWriteFile' function
// TODO - Need to add unit tests to the Csv module
module Csv =
    open System
    open System.IO

    type CsvFile =
        | WithHeaders of Map<string, string> array
        | WithoutHeaders of string array array

    // TODO - Enhance this function to account for escaped commas
    let private split (str : string) =
        str |> String.split [| ',' |] StringSplitOptions.RemoveEmptyEntries

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
                    let msg = $"Failed to create file '{file}' since it already exists"
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
                let msg = $"Failed to delete file '{file}' since it does not exist"
                Response.Failure { Code = 3; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            Response.Unhandled ex

    let readFile file useAsync hasHeaders =
        let read file =
            match useAsync with
            | true  -> FileOperations.readFileAsync file
            | false -> FileOperations.readFile file

        match File.Exists file with
        | true ->
            let contents = read file
            match hasHeaders with
            | true ->
                match contents |> Array.isEmpty with
                | true ->
                    WithHeaders Array.empty
                | false ->
                    let headers = contents |> Array.head |> split
                    match (headers |> Seq.distinct |> Seq.length) = (headers |> Array.length) with
                    | true ->
                        contents |> Array.tail
                        |> Array.Parallel.map (fun content ->
                            content
                            |> split
                            |> Array.Parallel.mapi (fun index element ->
                                headers[index], element)
                            |> Map.ofArray)
                        |> WithHeaders
                    | false ->
                        WithHeaders Array.empty
            | false ->
                contents
                |> Array.Parallel.map split
                |> WithoutHeaders
        | false ->
            match hasHeaders with
            | true  -> WithHeaders Array.empty
            | false -> WithoutHeaders Array.empty

    let tryReadFile file useAsync hasHeaders =
        try
            match File.Exists file with
            | true ->
                let contents = readFile file useAsync hasHeaders
                match contents with
                | WithHeaders inner ->
                    match inner |> Array.isEmpty with
                    | true ->
                        let fileSize = (FileInfo file).Length
                        match fileSize > 0L with
                        | true ->
                            let msg = $"Failed to read file '{file}' since the column headings are not unique"
                            ResponseWithValue.Failure { Code = 5; Type = IO; Message = msg; InnerException = None }
                        | false ->
                            ResponseWithValue.Success contents
                    | false ->
                        ResponseWithValue.Success contents
                | WithoutHeaders _ ->
                    ResponseWithValue.Success contents
            | false ->
                let msg = $"Failed to read file '{file}' since it does not exist"
                ResponseWithValue.Failure { Code = 3; Type = IO; Message = msg; InnerException = None }
        with
        | ex ->
            ResponseWithValue<_>.Unhandled ex

    let writeFile contents file useAsync overwrite =
        let getLines () =
            match contents with
            | WithHeaders inner ->
                raise (NotImplementedException ())
            | WithoutHeaders inner ->
                raise (NotImplementedException ())

        let write file lines =
            match useAsync with
            | true  -> FileOperations.writeFileAsync file lines
            | false -> FileOperations.writeFile file lines

        match File.Exists file && overwrite with
        | true  -> File.Delete file
        | false -> ()

        match File.Exists file with
        | true  -> ()
        | false -> write file (getLines ())
