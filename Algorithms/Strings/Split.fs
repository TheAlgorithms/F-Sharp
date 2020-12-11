namespace Strings

module Split =
    let split(str: string, separator: string) =
        let mutable newStringList = []
        let mutable value = ""
        if str.Contains separator then
            for c in str do
                match c.ToString() with
                | c when c.Contains separator -> newStringList <- newStringList |> List.append <| [value]; value <- ""
                | _ -> value <- value + c.ToString(); 
            if value <> "" then newStringList <- newStringList |> List.append <| [value]; value <- ""
        else newStringList <- [value]
        newStringList
