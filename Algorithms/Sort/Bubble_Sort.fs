namespace Algorithms.Sort

module BubbleSort =
    let rec Sort list: 'T [] =
        let mutable updated = false
        let mutable list = list |> Array.copy
        for index in 0 .. list.Length - 1 do
            if index < list.Length - 1 then
                let current = list.[index]
                let next = list.[index + 1]
                if next < current then
                    list.[index] <- next
                    list.[index + 1] <- current
                    updated <- true
        if updated then list <- Sort list
        list
