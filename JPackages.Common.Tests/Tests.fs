module JPackages.Common.Functions.Tests

open System
open System.IO
open System.Xml.XPath
open NUnit.Framework

open JPackages.Common.Domain
open JPackages.Common.Functions

let xmlUnitTest1 = Directory.GetCurrentDirectory () + "\\XmlUnitTest1.xml"
let xmlUnitTest2 = Directory.GetCurrentDirectory () + "\\XmlUnitTest2.xml"

[<SetUp>]
let Setup () =
    let xmlUnitTest1Contents = """<?xml version="1.0" encoding="UTF-8"?>
<Test>
  <NoValue/>
  <NoValue></NoValue>
  <Value>0.0</Value>
  <AnotherValue>1.0</AnotherValue>
  <Node a="value">
    <ChildValue>"Test"</ChildValue>
  </Node>
</Test>
"""
    use sw = new StreamWriter (xmlUnitTest1)
    sw.Write xmlUnitTest1Contents
    
    match File.Exists xmlUnitTest2 with
    | true  -> File.Delete xmlUnitTest2
    | false -> ()

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo2DP () =
    // Arrange
    let number = 0.885

    // Act
    let result = Math.round 2 number

    // Assert
    Assert.AreEqual(0.89, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo3DP () =
    // Arrange
    let number = 0.8854

    // Act
    let result = Math.round 3 number

    // Assert
    Assert.AreEqual(0.885, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo4DP () =
    // Arrange
    let number = 0.88546

    // Act
    let result = Math.round 4 number

    // Assert
    Assert.AreEqual(0.8855, result)
    
[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo2DPWithTrailingNines () =
    // Arrange
    let number = 0.884999999

    // Act
    let result = Math.round 2 number

    // Assert
    Assert.AreEqual(0.88, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo3SF () =
    // Arrange
    let number = 1.374985
    
    // Act
    let result = Math.roundToSignificantFigures 3 number
    
    // Assert
    Assert.AreEqual (1.37, result)
    
[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo4SF () =
    // Arrange
    let number = 0.00498591
    
    // Act
    let result = Math.roundToSignificantFigures 4 number
    
    // Assert
    Assert.AreEqual (0.004986, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_RoundUp_RoundNumberTo2DP () =
    // Arrange
    let number = 1.4832
    
    // Act
    let result = Math.roundUp 2 number
    
    // Assert
    Assert.AreEqual (1.49, result)
    
[<Test>]
[<Category("Rounding")>]
let Rounding_RoundUp_RoundNumberTo2SF () =
    // Arrange
    let number = 0.0761
    
    // Act
    let result = Math.round 3 (Math.roundUpToSignificantFigures 2 number)
    
    // Assert
    Assert.AreEqual (Math.round 3 0.077, result)
    
[<Test>]
[<Category("Rounding")>]
let Rounding_RoundDown_RoundNumberTo2DP () =
    // Arrange
    let number = 1.4832
    
    // Act
    let result = Math.roundDown 2 number
    
    // Assert
    Assert.AreEqual (1.48, result)
    
[<Test>]
[<Category("Rounding")>]
let Rounding_RoundDown_RoundNumberTo2SF () =
    // Arrange
    let number = 0.0761
    
    // Act
    let result = Math.round 3 (Math.roundDownToSignificantFigures 2 number)
    
    // Assert
    Assert.AreEqual (Math.round 3 0.076, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_First_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = Tuples.TuplesOfThree.first tuple

    // Assert
    Assert.AreEqual(10, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_Second_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = Tuples.TuplesOfThree.second tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_Third_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = Tuples.TuplesOfThree.third tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_First_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.first tuple

    // Assert
    Assert.AreEqual(10, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_Second_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.second tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_Third_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.third tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_Fourth_RetrievesFourthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.fourth tuple

    // Assert
    Assert.AreEqual(13, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_First_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.first tuple

    // Assert
    Assert.AreEqual(10, result)
    
[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_Second_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.second tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_Third_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.third tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_Fourth_RetrievesFourthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.fourth tuple

    // Assert
    Assert.AreEqual(13, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_Fifth_RetrievesFifthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.fifth tuple

    // Assert
    Assert.AreEqual(14, result)

(*[<Test>]
[<Category("Xml")>]
let Xml_ReadFile_ReadsFileFromDisk () =
    let actual = Xml.readFile xmlUnitTest1
    
    let node = (((actual |> List.head).Children |> List.head).Children |> List.head)
    
    Assert.AreEqual ("Test.Node", node.FullName)
    Assert.True node.HasAttributes
    
[<Test>]
[<Category("Xml")>]
let Xml_WriteFile_WritesFileToDisk () =
    let (contents : Xml.Node list) =
        [{ Namespace = ""
           NamespacePrefix = ""
           FullName = ""
           LocalName = ""
           Value = ""
           Attributes = Map.empty
           Children = [
               { Namespace = ""
                 NamespacePrefix = ""
                 FullName = "Test"
                 LocalName = "Test"
                 Value = ""
                 Attributes = Map.empty
                 Children = [
                     { Namespace = ""
                       NamespacePrefix = ""
                       FullName = "Test.Value"
                       LocalName = "Value"
                       Value = "0.0"
                       Attributes = Map [ "attribute1", "\"1.0\""; "attribute2", "\"2.0\"" ]
                       Children = List.empty
                       NodeType = XPathNodeType.Element }
                 ]
                 NodeType = XPathNodeType.Element }
           ]
           NodeType = XPathNodeType.Root }]
    
    Xml.writeFile xmlUnitTest2 contents
    
    Assert.True (File.Exists xmlUnitTest2)*)
