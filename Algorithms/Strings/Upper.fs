namespace Algorithms.Strings

module Upper =
    /// <summary>
    /// Will convert the entire string to uppercase letters
    /// </summary>
    /// <param name="input">String to change to uppercase.</param>
    /// <returns>Uppercased string</returns>
    let upper (input: string) =
        input.Split()
        |> Array.map
            (fun word ->
                word.ToCharArray()
                |> Array.map
                    (fun character ->
                        if character >= 'a' && character <= 'z' then
                            char (int character - 32)
                        else
                            character)
                |> (fun characters -> System.String.Concat(characters)))
        |> String.concat " "
