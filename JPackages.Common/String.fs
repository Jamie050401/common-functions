namespace JPackages.Common.Functions

open System

module String =
    let split (options : StringSplitOptions) (chars : char array) (str : string) = str.Split (chars, options)

    let insertWhitespace (padding : int) (str : string) =
        str.PadLeft ((str |> String.length) + padding)
