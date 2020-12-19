namespace Algorithms.Sort

module GnomeSort =
    let Sort list: 'T [] =
        let mutable list = list |> Array.copy
        let mutable first = 1
        let mutable second = 2
        while first < list.Length do
            if list.[first - 1] <= list.[first] then
                first <- second
                second <- second + 1
            else
                let tmp = list.[first - 1]
                list.[first - 1] <- list.[first]
                list.[first] <- tmp
                first <- first - 1
                if first = 0 then
                    first <- 1
                    second <- 2
        list
