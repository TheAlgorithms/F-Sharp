namespace Algorithms.Strings

module HasSuffix =
    /// <summary>
    /// Reports string has specified suffix or not.
    /// </summary>
    let HasSuffix (s: string, suffix: string) =
        s.Length >= suffix.Length && s.Substring(s.Length-suffix.Length) = suffix
