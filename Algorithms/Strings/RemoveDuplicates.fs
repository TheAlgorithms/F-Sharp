/// <summary>
/// Remove duplicates from sentence
/// <summary>
namespace Algorithms.Strings

module RemoveDuplicates =
    let removeDuplicates (string: string) =
        let mutable newString = ""
        for s in string.Split() do
            if not (newString.Contains s) then newString <- newString + " " + s
        newString
