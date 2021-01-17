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

        match secondWord.Length with
        | s when s > firstWord.Length -> levenshteinDistance (secondWord, firstWord)
        | 0 -> firstWord.Length
        | _ ->
            let mutable previousRow = [ 0 .. secondWord.Length ]

            firstWord
            |> Seq.iteri
                (fun i c1 ->
                    let mutable currentRow = [ i + 1 ]

                    secondWord
                    |> Seq.iteri
                        (fun j c2 ->
                            let insertions = previousRow.[j + 1] + 1
                            let deletions = currentRow.[j] + 1

                            let substitutions =
                                previousRow.[j] + (if c1 <> c2 then 1 else 0)

                            // Get the minimum to append to the current row
                            currentRow <-
                                currentRow
                                |> List.append [ (min insertions (min deletions substitutions)) ])

                    previousRow <- currentRow |> List.rev)

            previousRow |> List.rev |> List.item 0
