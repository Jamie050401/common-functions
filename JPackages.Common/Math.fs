namespace JPackages.Common.Functions

module Math =
    let private negate originalValue currentValue =
        match originalValue < 0.0 with
        | true  -> currentValue * -1.0
        | false -> currentValue

    let round (precision : int) number =
        (floor ((abs number + 0.5 / 10.0 ** precision) * 10.0 ** precision)) / 10.0 ** precision |> negate number

    let roundToSignificantFigures (precision : int) number =
        match number = 0.0 with
        | true  -> number
        | false ->
            let scale = 10.0 ** floor (log10 (abs number) + 1.0)
            scale * round precision (number / scale)

    let truncate (precision : int) number =
        (floor (abs number * 10.0 ** precision)) / 10.0 ** precision |> negate number

    let truncateToSignificantFigures (precision : int) number =
        match number = 0.0 with
        | true  -> number
        | false ->
            let scale = 10.0 ** floor (log10 (abs number) + 1.0)
            scale * truncate precision (number / scale)

    let isRoundingReqd (precision : int) number =
        abs number * 10.0 ** precision
        |> fun value -> round 1 (value - truncate 0 value) <> 0.0

    let roundUp (precision : int) number =
        match isRoundingReqd precision number with
        | true  -> (ceil (abs number * 10.0 ** precision)) / 10.0 ** precision |> negate number
        | false -> number

    let roundUpToSignificantFigures (precision : int) number =
        match number = 0.0 with
        | true  -> number
        | false ->
            let scale = 10.0 ** floor (log10 (abs number) + 1.0)
            scale * roundUp precision (number / scale)

    let roundDown (precision : int) number =
        match isRoundingReqd precision number with
        | true  -> (floor (abs number * 10.0 ** precision)) / 10.0 ** precision |> negate number
        | false -> number

    let roundDownToSignificantFigures (precision : int) number =
        match number = 0.0 with
        | true  -> number
        | false ->
            let scale = 10.0 ** floor (log10 (abs number) + 1.0)
            scale * roundDown precision (number / scale)
