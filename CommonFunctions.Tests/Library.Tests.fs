module JPackages.Common.Functions.Tests

open NUnit.Framework

open JPackages.Common.Functions

//[<SetUp>]
//let Setup () =
//    ()

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
[<Category("Tuples Of Three")>]
let TuplesOfThree_first_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = Tuples.TuplesOfThree.first tuple

    // Assert
    Assert.AreEqual(10, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_second_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = Tuples.TuplesOfThree.second tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_third_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = Tuples.TuplesOfThree.third tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_first_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.first tuple

    // Assert
    Assert.AreEqual(10, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_second_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.second tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_third_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.third tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_fourth_RetrievesFourthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = Tuples.TuplesOfFour.fourth tuple

    // Assert
    Assert.AreEqual(13, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_first_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.first tuple

    // Assert
    Assert.AreEqual(10, result)
    
[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_second_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.second tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_third_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.third tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_fourth_RetrievesFourthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.fourth tuple

    // Assert
    Assert.AreEqual(13, result)

[<Test>]
[<Category("Tuples Of Five")>]
let TuplesOfFive_fifth_RetrievesFifthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13, 14

    // Act
    let result = Tuples.TuplesOfFive.fifth tuple

    // Assert
    Assert.AreEqual(14, result)
