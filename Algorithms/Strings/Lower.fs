namespace Algorithms.Strings

module Lower =
    /// <summary>
    /// Will convert the entire string to lowercase letters
    /// </summary>
    /// <example>
    /// <code>
    /// lower("wow")
    /// 'wow'
    ///
    /// lower("HellZo")
    /// hellzo'
    ///
    /// lower("WHAT")
    /// 'what'
    ///
    /// lower("wh[]32")
    /// 'wh[]32'
    ///
    /// lower("whAT")
    /// 'what'
    /// </code>
    /// </example>
    /// <param name="input"></param>
    /// <returns></returns>
    let lower (input: string): string =
        input.Split()
        |> Array.map
            (fun word ->
                word.ToCharArray()
                |> Array.map
                    (fun character ->
                        if character >= 'A' && character <= 'Z' then
                            char (int character + 32)
                        else
                            character)
                |> (fun characters -> System.String.Concat(characters)))
        |> String.concat " "
