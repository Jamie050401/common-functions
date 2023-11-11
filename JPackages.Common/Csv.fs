namespace JPackages.Common.Functions

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
        |> Tuples.TuplesOfThree.third

    let readFile file =
        match File.Exists file with
        | true ->
            let contents = File.readFile file
            contents |> Array.Parallel.map split
        | false ->
            Array.empty

    let private getLines (contents : 'a array array) =
        contents |> Array.map (fun columns ->
            columns |> ("" |> Array.fold (fun str column ->
                match str |> String.IsNullOrEmpty with
                | true  -> column.ToString ()
                | false -> str + "," + column.ToString ())))
    
    let writeFile file contents =
        match File.Exists file with
        | true  -> File.Delete file
        | false -> ()

        File.writeLinesToFile file (getLines contents)
