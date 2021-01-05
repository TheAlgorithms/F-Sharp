namespace Algorithms.Strings

module IsPalindrome =
    /// <summary>
    /// Determine whether the string is palindrome
    /// </summary>
    /// <param name="str"></param>
    /// <returns>Boolean</returns>
    let isPalindrome (str: string): bool =
        let str =
            str |> Seq.filter ((<>) ' ') |> Seq.toList

        str = (str |> List.rev)
