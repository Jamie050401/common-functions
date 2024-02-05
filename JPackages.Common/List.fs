namespace JPackages.Common.Functions

module List =
    let filteri func list =
        let mutable index = -1

        list
        |> List.filter (fun element ->
            index <- index + 1
            func index element)

    let foldi func acc list =
        let mutable index = -1

        list
        |> (acc
            |> List.fold (fun acc element ->
                index <- index + 1
                func index acc element))
