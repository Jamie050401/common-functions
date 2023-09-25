namespace CommonFunctions

open System

type ErrorType =
    | Unhandled

type Error =
    { Code : int
      Type : ErrorType
      Message : string
      InnerException : exn option }

    static member Default =
        { Code = 1
          Type = Unhandled
          Message = ""
          InnerException = None }

    static member Unhandled code ex =
        { Code = code
          Type = Unhandled
          Message = "Unhandled exception"
          InnerException = Some ex }

[<RequireQualifiedAccess>]
type Response =
    | Success
    | Failure of Error

[<RequireQualifiedAccess>]
type ResponseWithValue<'a> =
    | Success of 'a
    | Failure of Error

module Maths =
    let round (number:float) (decimalPlaces:int) = Math.Round(number + 0.00000001, decimalPlaces)
    let round2dp (number:float) = round number 2

module TuplesOfThree =
    let fst (tuple:'a*'b*'c) = match tuple with | value, _, _ -> value
    let snd (tuple:'a*'b*'c) = match tuple with | _, value, _ -> value
    let thrd (tuple:'a*'b*'c) = match tuple with | _, _, value -> value

module TuplesOfFour =
    let fst (tuple:'a*'b*'c*'d) = match tuple with | value, _, _, _ -> value
    let snd (tuple:'a*'b*'c*'d) = match tuple with | _, value, _, _ -> value
    let thrd (tuple:'a*'b*'c*'d) = match tuple with | _, _, value, _ -> value
    let frth (tuple:'a*'b*'c*'d) = match tuple with | _, _, _, value -> value