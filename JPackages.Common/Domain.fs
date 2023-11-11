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
    open System.Xml.XPath

    type Node =
        { Namespace : string
          NamespacePrefix : string
          FullName : string
          LocalName : string
          Value : string
          Attributes : Map<string, string>
          Children : Node list
          NodeType : XPathNodeType }
        
        member this.HasAttributes =
            not (this.Attributes |> Map.isEmpty)
            
        member this.HasChildren =
            not (this.Children |> List.isEmpty)
            
        member this.ToString (children : string) =
            let attributes =
                match this.Attributes |> Map.isEmpty with
                | true  -> ""
                | false ->
                    this.Attributes |> (" " |> Map.fold (fun attributes attributeName attributeValue ->
                        attributes + attributeName + "=" + attributeValue + " "))
            "<" + this.LocalName + attributes + ">" + this.Value + children + "</" + this.LocalName + ">"
