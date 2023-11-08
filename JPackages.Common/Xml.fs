namespace JPackages.Common.Functions

open JPackages.Common.Domain

module Xml =
    open System
    open System.Xml.Linq
    open System.Xml.XPath
    
    let private getValue (xNavigator : XPathNavigator) =
        let innerXml = xNavigator.InnerXml
        match innerXml.Contains "<" with
        | false -> xNavigator.Value
        | true  ->
            match innerXml.IndexOf "<" > 0 with
            | false -> ""
            | true  ->
                innerXml.ToCharArray ()
                |> (("", false)
                |> Array.fold (fun (newValue, foundChild) char ->
                    match foundChild with
                    | true  -> newValue, foundChild
                    | false ->
                        let hasFoundChild = (char = '<')
                        match hasFoundChild with
                        | true  -> newValue, hasFoundChild
                        | false -> newValue + string char, hasFoundChild))
                |> fst
    
    let private getAttributes (xNavigator : XPathNavigator) : Map<string, string> =
        let xAttrNavigator = xNavigator.CreateNavigator ()
        
        let rec readAttributes (attributes : Map<string, string>) =
            match xAttrNavigator.MoveToNextAttribute () with
            | false ->
                match xAttrNavigator.MoveToFirstAttribute () with
                | false -> attributes
                | true  -> (attributes |> Map.add xAttrNavigator.LocalName xAttrNavigator.Value) |> readAttributes
            | true  ->
                (attributes |> Map.add xAttrNavigator.LocalName xAttrNavigator.Value) |> readAttributes
        
        readAttributes Map.empty
    
    let private getNode (xNavigator : XPathNavigator) (partialFullName : string) (childNodes : Xml.Node list) : Xml.Node =
        { Namespace = xNavigator.NamespaceURI
          NamespacePrefix = xNavigator.Prefix
          FullName =
              match partialFullName |> String.IsNullOrWhiteSpace with
              | true  -> xNavigator.Name
              | false -> partialFullName + "." + xNavigator.Name
          LocalName = xNavigator.LocalName
          Value = getValue xNavigator
          Attributes = getAttributes xNavigator
          Children = childNodes
          NodeType = xNavigator.NodeType }
    
    let readFile (file : string) =
        let xDoc = XPathDocument file
        let xNavigator = xDoc.CreateNavigator ()
        
        let rec readXml (xNavigator : XPathNavigator) (partialFullName : string) (nodes : Xml.Node list) : Xml.Node list =
            let moveToNext partialFullName newNodes =
                match xNavigator.MoveToNext () with
                | false -> newNodes
                | true  -> readXml xNavigator partialFullName newNodes
            
            match xNavigator.HasChildren with
            | false ->
                let newNodes =
                    match (xNavigator.NodeType = XPathNodeType.Element) with
                    | false -> nodes
                    | true  -> getNode xNavigator partialFullName List.empty :: nodes
                moveToNext partialFullName newNodes
            | true  ->
                let xChildNavigator = xNavigator.CreateNavigator ()
                xChildNavigator.MoveToFirstChild () |> ignore
                match (xChildNavigator.NodeType = XPathNodeType.Element) with
                | false ->
                    let newNodes = getNode xNavigator partialFullName List.empty :: nodes
                    moveToNext partialFullName newNodes
                | true  ->
                    let newPartialFullName =
                        match partialFullName |> String.IsNullOrWhiteSpace with
                        | true  -> partialFullName + xNavigator.LocalName
                        | false -> partialFullName + "." + xNavigator.LocalName
                    let childNodes = readXml xChildNavigator newPartialFullName List.empty
                    let newNodes = getNode xNavigator partialFullName childNodes :: nodes
                    moveToNext partialFullName newNodes
        
        readXml xNavigator "" List.empty
    
    let writeFile file (nodes : Xml.Node list) =
        let rec convertToString (contents : string) (nodes : Xml.Node list) =
            match nodes with
            | node :: nodes ->
                match node.HasChildren with
                | false ->
                    let contents = contents + node.ToString ""
                    convertToString contents nodes
                | true  ->
                    let children = node.Children |> convertToString ""
                    let contents =
                        match (node.NodeType = XPathNodeType.Root) with
                        | true  -> contents + children
                        | false -> contents + node.ToString children
                    convertToString contents nodes
            | [] ->
                contents

        let contents =
            convertToString "" nodes
            |> XDocument.Parse
            |> fun xDoc ->
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + Environment.NewLine + xDoc.ToString () + Environment.NewLine
        File.write file contents
