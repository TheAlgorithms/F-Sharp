// wiki: https://en.wikipedia.org/wiki/Anagram

namespace Algorithms.Strings

module CheckAnagrams =
    /// <summary>
    /// Two strings are anagrams if they are made of the same letters
    /// arranged differently (ignoring the case).
    /// </summary>
    /// <param name="string1">First string</param>
    /// <param name="string2">Second string</param>
    /// <returns>Boolean</returns>
    let isAnagram (string1: string, string2: string) =
        let a =
            string1.ToCharArray() |> Array.sortBy (id)

        let b =
            string2.ToCharArray() |> Array.sortBy (id)

        a = b
