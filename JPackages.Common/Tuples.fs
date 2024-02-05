namespace JPackages.Common.Functions

module Tuples =
    module TuplesOfThree =
        let first tuple =
            match tuple with
            | value, _, _ -> value

        let second tuple =
            match tuple with
            | _, value, _ -> value

        let third tuple =
            match tuple with
            | _, _, value -> value

    module TuplesOfFour =
        let first tuple =
            match tuple with
            | value, _, _, _ -> value

        let second tuple =
            match tuple with
            | _, value, _, _ -> value

        let third tuple =
            match tuple with
            | _, _, value, _ -> value

        let fourth tuple =
            match tuple with
            | _, _, _, value -> value

    module TuplesOfFive =
        let first tuple =
            match tuple with
            | value, _, _, _, _ -> value

        let second tuple =
            match tuple with
            | _, value, _, _, _ -> value

        let third tuple =
            match tuple with
            | _, _, value, _, _ -> value

        let fourth tuple =
            match tuple with
            | _, _, _, value, _ -> value

        let fifth tuple =
            match tuple with
            | _, _, _, _, value -> value
