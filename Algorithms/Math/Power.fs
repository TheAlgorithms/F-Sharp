namespace Algorithms.Math

module Power =
    let Pow x powerOf =
        [1..powerOf] |> Seq.fold (fun acc _ -> acc * x) 1