namespace Algorithms.Math

module Factorial =
    /// Calculates factorial. Time complexity: O(n)
    let byFoldFunction n =
        if n < 0 then
            failwith "No factorial for negative numbers"
        else
            { 1 .. n } |> Seq.fold (fun acc n -> acc * n) 1

    /// Calculates factorial. Time complexity: O(n)
    let byReduceFunction n =
        match n with
        | n when n < 0 -> failwith "No factorial for negative numbers"
        | 0 -> 1
        | n -> { 1 .. n } |> Seq.reduce (*)

    /// Calculates factorial. Time complexity: O(n)
    let rec byRecursion n =
        match sign n with
        | -1 -> failwith "No factorial for negative numbers"
        | 0 -> 1
        | _ -> n * byRecursion (n - 1)

    /// Calculates factorial. Time complexity: O(n)
    let byTailRecursion n =
        let rec inner n prod =
            match n with
            | 0 -> prod
            | _ -> inner (n - 1) (prod * n)

        match n with
        | n when n < 0 -> failwith "No factorial for negative numbers"
        | _ -> inner n 1

    /// Calculates factorial. Time complexity: O(n)
    let inline byTailRecursionGeneric n =
        let gen0 = LanguagePrimitives.GenericZero
        let gen1 = LanguagePrimitives.GenericOne

        let rec inner n prod =
            match n with
            | n when n = gen0 -> prod
            | _ -> inner (n - gen1) (prod * n)

        match n with
        | n when n < gen0 -> failwith "No factorial for negative numbers"
        | _ -> inner n gen1
