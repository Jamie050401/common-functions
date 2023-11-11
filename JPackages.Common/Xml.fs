namespace JPackages.Common.Functions

module Xml =
    open System.IO
    open System.Xml.Linq

    let readFile (file : string) =
        match File.Exists file with
        | true ->
            let xDoc = XDocument.Load file
            xDoc.Elements () |> List.ofSeq
        | false ->
            List.empty

    let writeFile (file : string) (contents : XElement list) =
        let formatted =
            let parentName = try (contents |> List.head).Parent.Name.LocalName with | _ -> "Root"
            (contents |> (("<" + parentName + ">") |> List.fold (fun str element ->
                str + element.ToString ()))) + "</" + parentName + ">"
            
        File.writeFile file formatted
    
    let private getValue (element : XElement) =
        match element.HasElements with
        | true  -> element.Value.Replace (element.Elements () |> ("" |> Seq.fold (fun str element -> str + element.Value)), "")
        | false -> element.Value
        
    let findElement (contents : XElement list) (name : string) =
        contents |> List.find (fun element ->
            element.Element name <> null)

    let tryFindElement (contents : XElement list) (name : string) =
        contents |> List.tryFind (fun element ->
            element.Element name <> null)
        
    let findElementWithValue (contents : XElement list) (value : string) =
        contents |> List.find (fun element ->
            getValue element = value)
        
    let tryFindElementWithValue (contents : XElement list) (value : string) =
        contents |> List.tryFind (fun element ->
            getValue element = value)
