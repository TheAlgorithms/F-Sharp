
namespace Algorithms.Strings

module RemoveDuplicates =
    /// <summary>
    /// Remove duplicates from sentence
    /// </summary>
    let removeDuplicates (string: string) =
        let mutable newString = ""
        for s in string.Split() do
            if not (newString.Contains s) then newString <- newString + " " + s
        newString
