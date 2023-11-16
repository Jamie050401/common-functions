namespace JPackages.Common.Domain

type ErrorType =
    | Unhandled
    | IO
    | Value
    | Argument

// Error Codes:
//   0 - Default
//   1 - Unhandled
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

module Xml =
    open System
    open System.Xml

    type Node =
        { Namespace : string
          NamespacePrefix : string
          LocalName : string
          Value : string
          Attributes : Map<string, string>
          Children : Node list
          NodeType : XmlNodeType }

        member this.HasAttributes =
            not (this.Attributes |> Map.isEmpty)

        member this.HasChildren =
            not (this.Children |> List.isEmpty)

        member this.ToString (?includeHeader : bool) =
            let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            let namespacePrefix =
                match this.NamespacePrefix |> String.IsNullOrWhiteSpace with
                | true  -> ""
                | false -> this.NamespacePrefix + ":"
            let attributes =
                match this.Attributes |> Map.isEmpty with
                | true  -> ""
                | false ->
                    this.Attributes |> (" " |> Map.fold (fun attributes attributeName attributeValue ->
                        match attributes |> String.IsNullOrWhiteSpace with
                        | true  -> attributes + attributeName + "=" + attributeValue
                        | false -> attributes + " " + attributeName + "=" + attributeValue))
            let children =
                this.Children |> ("" |> List.fold (fun str node -> str + node.ToString false))
            match includeHeader.IsSome with
            | true ->
                match includeHeader.Value with
                | true  -> header + "<" + namespacePrefix + this.LocalName + attributes + ">" + this.Value + children + "</" + namespacePrefix + this.LocalName + ">"
                | false -> "<" + namespacePrefix + this.LocalName + attributes + ">" + this.Value + children + "</" + namespacePrefix + this.LocalName + ">"
            | false     -> "<" + namespacePrefix + this.LocalName + attributes + ">" + this.Value + children + "</" + namespacePrefix + this.LocalName + ">"
