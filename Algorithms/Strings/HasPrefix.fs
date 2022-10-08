namespace Algorithms.Strings

module HasPrefix =
    /// <summary>
    /// Reports string has specified prefix or not.
    /// </summary>
    let HasPrefix (s: string, prefix: string): bool =
        s.Length >= prefix.Length && s.Substring(0, prefix.Length) = prefix
 
