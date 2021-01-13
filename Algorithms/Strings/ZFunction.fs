///https://cp-algorithms.com/string/z-function.html
///
///Z-function or Z algorithm
///
///Efficient algorithm for pattern occurrence in a string
///
///Time Complexity: O(n) - where n is the length of the string
namespace Algorithms.Strings

module ZFunction =

    let goNext (i, zResult: array<int>, s: string) =
        i + zResult.[i] < s.Length
        && s.[zResult.[i]] = s.[i + zResult.[i]]

    /// <summary>
    /// For the given string this function computes value for each index,
    /// which represents the maximal length substring starting from the index
    /// and is the same as the prefix of the same size
    /// </summary>
    /// <param name="inputString"></param>
    /// <returns></returns>
    let zFunction (inputString: string): list<int> =
        let mutable zResult =
            [| for i in 1 .. inputString.Length -> 0 |]
        // Initialize interval's left pointer and right pointer
        let mutable leftPointer, rightPointer = 0, 0

        for i in 1 .. inputString.Length - 1 do
            // Case when current index is inside the interval
            if i <= rightPointer then
                let minEdge =
                    min (rightPointer - i + 1) (zResult.[i - leftPointer])

                zResult.SetValue(minEdge, i)

            while goNext (i, zResult, inputString) do
                zResult.[i] <- zResult.[i] + 1

            // if new index's result gives us more right interval,
            // we've to update left_pointer and right_pointer
            if i + zResult.[i] - 1 > rightPointer then
                leftPointer <- i
                rightPointer <- i + zResult.[i] - 1

        zResult |> List.ofArray


    let findPattern (pattern: string, inputString: string): int =
        let mutable answer = 0
        // Concatenate 'pattern' and 'input_str' and call z_function
        // with concatenated string
        let zResult = zFunction (pattern + inputString)

        for value in zResult do
            // If value is greater then length of the pattern string
            // that means this index is starting position of substring
            // which is equal to pattern string
            if value >= pattern.Length then
                answer <- answer + 1

        answer
