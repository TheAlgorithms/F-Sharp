namespace Algorithms.Strings

/// wiki: https://en.wikipedia.org/wiki/Anagram
module CheckAnagrams =
    /// <summary>
    /// Two strings are anagrams if they are made of the same letters
    /// arranged differently (ignoring the case).
    /// </summary>
    /// <example>
    /// <code>
    /// check_anagrams('Silent', 'Listen')
    /// True
    ///
    /// check_anagrams('This is a string', 'Is this a string')
    /// True
    ///
    /// check_anagrams('This is    a      string', 'Is     this a string')
    /// True
    ///
    /// check_anagrams('There', 'Their')
    /// False
    /// </code>
    /// </example>
    /// <param name="string1">First string</param>
    /// <param name="string2">Second string</param>
    /// <returns>Boolean</returns>
    let isAnagram (string1: string, string2: string): bool =
        let a =
            string1.ToLower().ToCharArray()
            |> Array.filter (fun chars -> chars <> ' ')
            |> Array.sort
            |> System.String.Concat

        let b =
            string2.ToLower().ToCharArray()
            |> Array.filter (fun chars -> chars <> ' ')
            |> Array.sort
            |> System.String.Concat

        a = b
