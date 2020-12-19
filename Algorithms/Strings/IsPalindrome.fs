namespace Algorithms.Strings

module IsPalindrome =
    /// <summary>
    /// Determine whether the string is palindrome
    /// </summary>
    /// <param name="s"></param>
    /// <returns>Boolean</returns>
    let isPalindrome (s: string): bool =
        let refS = ref s
        refS
        := [ for character in s.ToLower() -> (string) character ]
        |> List.fold (+) ""
        string (refS.Value.ToCharArray() |> Array.rev) = refS.Value
