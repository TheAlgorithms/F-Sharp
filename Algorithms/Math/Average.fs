namespace Algorithms.Math

module Average =
    let inline average (xs: ^a list) =
        match xs with
        | [] -> None
        | _ ->
            let sum, count = List.fold (fun (sum, count) current -> (sum + current, count + 1))
                                       (LanguagePrimitives.GenericZero, 0)
                                       xs
            LanguagePrimitives.DivideByInt sum count |> Some                              
