module CommonFunctions.Tests

open NUnit.Framework
open CommonFunctions

//[<SetUp>]
//let Setup () =
//    ()

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo2DP () =
    // Arrange
    let number = 0.885

    // Act
    let result = Maths.round number 2

    // Assert
    Assert.AreEqual(0.89, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo3DP () =
    // Arrange
    let number = 0.8854

    // Act
    let result = Maths.round number 3

    // Assert
    Assert.AreEqual(0.885, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_Round_RoundNumberTo4DP () =
    // Arrange
    let number = 0.88546

    // Act
    let result = Maths.round number 4

    // Assert
    Assert.AreEqual(0.8855, result)

[<Test>]
[<Category("Rounding")>]
let Rounding_Round2dp_RoundNumberTo2DP () =
    // Arrange
    let number = 0.88546

    // Act
    let result = Maths.round2dp number

    // Assert
    Assert.AreEqual(0.89, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_fst_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = TuplesOfThree.fst tuple

    // Assert
    Assert.AreEqual(10, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_snd_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = TuplesOfThree.snd tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Three")>]
let TuplesOfThree_thrd_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12

    // Act
    let result = TuplesOfThree.thrd tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_fst_RetrievesFirstValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = TuplesOfFour.fst tuple

    // Assert
    Assert.AreEqual(10, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_snd_RetrievesSecondValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = TuplesOfFour.snd tuple

    // Assert
    Assert.AreEqual(11, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_thrd_RetrievesThirdValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = TuplesOfFour.thrd tuple

    // Assert
    Assert.AreEqual(12, result)

[<Test>]
[<Category("Tuples Of Four")>]
let TuplesOfFour_frth_RetrievesFourthValueFromTuple () =
    // Arrange
    let tuple = 10, 11, 12, 13

    // Act
    let result = TuplesOfFour.frth tuple

    // Assert
    Assert.AreEqual(13, result)
