namespace Algorithms.Sort

module CycleSort =
    let Sort list: 'T [] =
        let mutable list = list |> Array.copy
        let mutable writes = 0
        for index in 0 .. list.Length - 1 do
            let mutable value = list.[index]
            let mutable pos = index
            for i in index + 1 .. list.Length - 1 do
                if list.[i] < value then pos <- pos + 1
            if pos <> index then
                while value = list.[pos] do
                    pos <- pos + 1
                let mutable tmp = list.[pos]
                list.[pos] <- value
                value <- tmp
                writes <- writes + 1
                while pos <> index do
                    pos <- index
                    for i in index + 1 .. list.Length - 1 do
                        if list.[i] < value then pos <- pos + 1
                    while value = list.[pos] do
                        pos <- pos + 1
                    tmp <- list.[pos]
                    list.[pos] <- value
                    value <- tmp
                    writes <- writes + 1
        list
