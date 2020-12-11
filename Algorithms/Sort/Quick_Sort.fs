namespace Algorithms.Sort

module QuickSort =
    let Sort lst =
      let rec aux l cont =
        match l with
        | [] -> cont []
        | pivot::rest ->
          let left, right = rest |> List.partition(fun i -> i < pivot)
          aux left (fun acc_left ->
          aux right (fun acc_right -> cont(acc_left @ pivot::acc_right)))
      aux lst (fun x -> x)