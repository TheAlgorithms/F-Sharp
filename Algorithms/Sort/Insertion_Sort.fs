namespace Algorithms.Sort

module InsertionSort =
    let Sort list: 'T [] =
        let mutable list = list |> Array.copy
        for index in 1 .. list.Length - 1 do
            let item = list.[index]
            let mutable j = index
            while j > 0 && list.[j - 1] > item do
                list.[j] <- list.[j - 1]
                j <- j - 1
            list.[j] <- item
        list
