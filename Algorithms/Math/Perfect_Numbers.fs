namespace Algorithms.Math

module Perfect_Numbers =
    let IsPerfect (number: int) =
        let total =
            seq { 1 .. number - 1 }
            |> Seq.filter (fun n -> number % n = 0)
            |> Seq.sum

        total = number
