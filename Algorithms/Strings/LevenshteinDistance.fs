namespace Algorithms.Strings

module LevenshteinDistance =
    /// <summary>
    /// Implementation of the levenshtein distance in F#.
    /// </summary>
    /// <param name="firstWord">The first word to measure the difference.</param>
    /// <param name="secondWord">The second word to measure the difference.</param>
    /// <returns></returns>
    let rec levenshteinDistance (firstWord: string, secondWord: string): int =
        // The longer word should come first
        // if firstWord.Length < secondWord.Length then
        //     levenshteinDistance(second_word, first_word)

        // if secondWord.Length = 0 then
        //     return len(first_word)
        match secondWord.Length with
        | s when s > firstWord.Length -> levenshteinDistance(secondWord, firstWord)
        | 0 -> firstWord.Length
        | _ ->
            let mutable previousRow = [secondWord.Length + 1]

            for i in 0 .. firstWord.Length - 1 do
                let c1 = firstWord.[i]

                let mutable currentRow = [i + 1]

                for j in 0 .. secondWord.Length do
                    let c2 = secondWord.[j]

                    // Calculate insertions, deletions and substitutions
                    let insertions = previousRow.[j + 1] + 1
                    let deletions = currentRow.[j] + 1
                    let substitutions = previousRow.[j]

                    // Get the minimum to append to the current row
                    currentRow <- currentRow |> List.append [(min insertions (min deletions substitutions))]

                previousRow <- currentRow
            previousRow.[-1]


