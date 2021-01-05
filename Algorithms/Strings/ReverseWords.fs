namespace Algorithms.Strings

module ReverseWords =
    /// <summary>
    /// Reverses words in a given string
    /// </summary>
    /// <param name="input">String to reverse.</param>
    /// <returns>Reversed string</returns>
    let reverseWords (input: string) =
        input.Split() |> Seq.rev |> String.concat " "
