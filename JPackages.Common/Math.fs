namespace JPackages.Common.Functions

open JPackages.Common.Domain

module Math =
    open System
    
    let private rnd (``type`` : RoundingType) (precision : int) (number : float) =
        let round number (precision : int) =
            (floor ((abs number + 0.5 / 10.0 ** precision) * 10.0 ** precision)) / 10.0 ** precision
            
        let truncate number (precision : int) =
            (floor (abs number * 10.0 ** precision)) / 10.0 ** precision
        
        let isRoundingReqd number (precision : int) =
            abs number * 10.0 ** precision
            |> fun value -> round (value - truncate value 0) 1 <> 0.0
        
        let roundUp number (precision : int) =
            let isRoundingReqd = isRoundingReqd number precision
            match isRoundingReqd with
            | true  -> (ceil (abs number * 10.0 ** precision)) / 10.0 ** precision
            | false -> number
            
        let roundDown number (precision : int) =
            let isRoundingReqd = isRoundingReqd number precision
            match isRoundingReqd with
            | true  -> (floor (abs number * 10.0 ** precision)) / 10.0 ** precision
            | false -> number
        
        match ``type`` with
        | RoundingType.Decimal behaviour ->
            match behaviour with
            | RoundingBehaviour.Standard -> round number precision
            | RoundingBehaviour.Truncate -> truncate number precision
            | RoundingBehaviour.Up       -> roundUp number precision
            | RoundingBehaviour.Down     -> roundDown number precision
        | RoundingType.Significant behaviour ->
            behaviour |> ignore
            raise (NotImplementedException ())
        |> fun rounded ->
            match number < 0.0 with
            | true  -> rounded * -1.0
            | false -> rounded
            
    let round (precision : int) (number : float) =
        rnd (RoundingType.Decimal RoundingBehaviour.Standard) precision number
        
    let truncate (precision : int) (number : float) =
        rnd (RoundingType.Decimal RoundingBehaviour.Truncate) precision number
