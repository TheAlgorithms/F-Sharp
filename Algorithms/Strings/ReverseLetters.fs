namespace Algorithms.Strings

module ReverseLetters =
    /// <summary>
    /// Reverses letters in a given string without adjusting the position of the words
    /// </summary>
    /// <param name="input">String to reverse.</param>
    /// <returns>Reversed string</returns>
    let reverseLetters (input: string) =
        input.Split()
        |> Array.map
            (fun x ->
                x.ToCharArray()
                |> Array.rev
                |> (fun c -> System.String.Concat(c)))
        |> String.concat " "
