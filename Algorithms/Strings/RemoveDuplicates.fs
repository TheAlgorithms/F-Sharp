namespace Algorithms.Strings

module RemoveDuplicates =
    /// <summary>
    /// Remove duplicates from sentence
    /// </summary>
    let removeDuplicates (str: string) =
        str.Split() |> Array.distinct |> String.concat " "
