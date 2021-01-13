namespace Algorithms.Strings

type Split() =
    /// <summary>
    /// Will split the string up into all the values separated by the separator
    /// </summary>
    /// <param name="str">String to split into lists</param>
    /// <param name="separator">The separator on what you would like to separate your strings</param>
    /// <returns>A string list</returns>
    static member Split(str: string, ?separator0: char): string list =
        let mutable newStringList: string list = []
        let separator = defaultArg separator0 ' '
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
