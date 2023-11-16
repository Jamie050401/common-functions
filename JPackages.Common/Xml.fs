namespace JPackages.Common.Functions

module Xml =
    open System.IO
    open System.Xml.Linq

    open JPackages.Common.Domain

    let private getValue (element : XElement) =
        match element.HasElements with
        | true  -> element.Value.Replace (element.Elements () |> ("" |> Seq.fold (fun str element -> str + element.Value)), "")
        | false -> element.Value

    let private constructNode (children : Xml.Node list) (element : XElement) =
        let elementName = element.Name
        let elementNamespace = elementName.Namespace
        let elementNamespacePrefix = element.GetPrefixOfNamespace elementNamespace.NamespaceName
        let elementValue = getValue element
        let elementAttributes = element.Attributes () |> Seq.map (fun attribute -> attribute.Name.LocalName, attribute.Value) |> Map.ofSeq
        { Namespace = elementNamespace.NamespaceName
          NamespacePrefix = elementNamespacePrefix
          LocalName = elementName.LocalName
          Value = elementValue
          Attributes = elementAttributes
          Children = children
          NodeType = element.NodeType } : Xml.Node

    let rec private readChildren (nodes : Map<int, Xml.Node list>) (index : int) (parents : Map<int, XElement>) (elements : Map<int, XElement list>) : Xml.Node =
        let children = elements.[index]
        match children with
        | [] ->
            let newElements = elements |> Map.remove index
            let parent = parents.[index]
            let children = nodes.[index]
            match index = 0 with
            | true  ->
                constructNode children parent
            | false ->
                let currentNodes = nodes.[index - 1]
                let newNodes = nodes |> Map.add (index - 1) (constructNode children parent :: currentNodes)
                readChildren (newNodes |> Map.remove index) (index - 1) (parents |> Map.remove index) newElements
        | child :: children ->
            let newElements = elements |> Map.add index children
            match child.HasElements with
            | true  ->
                readChildren nodes (index + 1) (parents |> Map.add (index + 1) child) (newElements |> Map.add (index + 1) (child.Elements () |> List.ofSeq))
            | false ->
                let currentNodes = try nodes.[index] with | _ -> List.empty
                let newNodes = nodes |> Map.add index (constructNode List.empty child :: currentNodes)
                readChildren newNodes index parents newElements

    let readFile (file : string) =
        let xDoc = XDocument.Load file
        xDoc.Elements () |> Seq.head |> fun element ->
            match element.HasElements with
            | true ->
                readChildren Map.empty 0 (Map [ 0, element ]) (Map [ 0, element.Elements () |> List.ofSeq ])
            | false ->
                constructNode List.empty element
        
    let tryReadFile (file : string) =
        try
            readFile file |> Some
        with
        | _ ->
            None

    let writeFile (file : string) (contents : Xml.Node) =
        File.writeFile file (contents.ToString true)
