namespace Algorithms.Sort

module MergeSort =
    let split list =
        let rec aux l acc1 acc2 =
            match l with
            | [] -> (acc1, acc2)
            | [ x ] -> (x :: acc1, acc2)
            | x :: y :: tail -> aux tail (x :: acc1) (y :: acc2)

        aux list [] []

    let rec merge l1 l2 =
        match (l1, l2) with
        | (x, []) -> x
        | ([], y) -> y
        | (x :: tx, y :: ty) -> if x <= y then x :: merge tx l2 else y :: merge l1 ty

    let rec sort list =
        match list with
        | [] -> []
        | [ x ] -> [ x ]
        | _ ->
            let (l1, l2) = split list
            merge (sort l1) (sort l2)
