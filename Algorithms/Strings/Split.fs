namespace Algorithms.Strings

module Split =
    /// <summary>
    /// Will split the string up into all the values separated by the separator
    /// </summary>
    /// <param name="str">String to split into lists</param>
    /// <param name="item">The separator on what you would like to separate your strings</param>
    /// <returns>A string list</returns>
    let split (str: string, separator: string) =
        let mutable newStringList = []
        let mutable value = ""
        if str.Contains separator then
            for c in str do
                match (string) c with
                | c when c.Contains separator ->
                    newStringList <- newStringList |> List.append <| [ value ]
                    value <- ""
                | _ -> value <- value + (string) c
            if value <> "" then
                newStringList <- newStringList |> List.append <| [ value ]
                value <- ""
        else
            newStringList <- [ value ]
        newStringList
