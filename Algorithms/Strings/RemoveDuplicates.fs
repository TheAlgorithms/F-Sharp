namespace Algorithms.Strings

/// <summary>
/// Remove duplicates from sentence
/// <summary>
module RemoveDuplicates =
    let removeDuplicates (string: string) =
        let mutable newString = ""
        for s in string.Split() do
            if not (newString.Contains s) then
                newString <- newString + " " + s
        newString