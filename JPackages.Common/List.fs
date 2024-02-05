namespace JPackages.Common.Functions

module List =
    let filteri func list =
        let mutable index = -1

        list
        |> List.filter (fun element ->
            index <- index + 1
            func index element)
