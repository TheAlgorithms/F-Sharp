namespace Strings

open Microsoft.FSharp.Collections

module RemoveDuplicates =
    let removeDuplicates (string: string) =
        let mutable newString = ""
        for s in string.Split() do
            if not (newString.Contains s) then
                newString <- newString + " " + s
        newString