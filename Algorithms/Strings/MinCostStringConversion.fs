/// Algorithm for calculating the most cost-efficient sequence for converting one string
/// into another.
/// The only allowed operations are
/// --- Cost to copy a character is copy_cost
/// --- Cost to replace a character is replace_cost
/// --- Cost to delete a character is delete_cost
/// --- Cost to insert a character is insert_cost
///
namespace Algorithms.Strings

module MinCostStringConversion =

    [<RequireQualifiedAccess>]
    type Operation =
        | Copy of char
        | Replace of Source: char * Target: char
        | Delete of char
        | Insert of char

    let computeTransformTables
        (
            source: string,
            destination: string,
            copyCost: int,
            replaceCost: int,
            deleteCost: int,
            insertCost: int
        ): array<array<int>> * array<array<Operation>> =

        let costs =
            Array.init (source.Length + 1) (fun  _ -> Array.init (destination.Length + 1) (fun _ -> 0))

        let ops =
            Array.init (source.Length + 1) (fun  _ -> Array.init (destination.Length + 1) (fun _ -> Operation.Copy 'a'))

        for i = 1 to source.Length do
            costs.[i].[0] <- i * deleteCost
            ops.[i].[0] <- Operation.Delete source.[i - 1]

        for i = 1 to destination.Length do
            costs.[0].[i] <- i * insertCost
            ops.[0].[i] <- Operation.Insert destination.[i - 1]

        for i in 1 .. source.Length do
            for j in 1 .. destination.Length do
                if source.[i - 1] = destination.[j - 1] then
                    costs.[i].[j] <- costs.[i - 1].[j - 1] + copyCost
                    ops.[i].[j] <- Operation.Copy (source.[i - 1])
                else
                    costs.[i].[j] <- costs.[i - 1].[j - 1] + replaceCost
                    ops.[i].[j] <- Operation.Replace (source.[i - 1], destination.[j - 1])

                if costs.[i - 1].[j] + deleteCost < costs.[i].[j] then
                    costs.[i].[j] <- costs.[i - 1].[j] + deleteCost
                    ops.[i].[j] <- Operation.Delete (source.[i - 1])

                if costs.[i].[j - 1] + insertCost < costs.[i].[j] then
                    costs.[i].[j] <- costs.[i].[j - 1] + insertCost
                    ops.[i].[j] <- Operation.Insert destination.[j - 1]

        costs, ops

    let rec assembleTransformation (ops: array<array<Operation>>, i: int, j: int): array<Operation> =
        printfn $"i={i},j={j},%A{ops}"
        if i = 0 && j = 0 then
            Array.empty
        else
            match ops.[i].[j] with
            | Operation.Replace _
            | Operation.Copy _ ->
                let seq = assembleTransformation (ops, i - 1, j - 1)
                Array.append seq [| ops[i][j] |]
            | Operation.Delete _ ->
                let seq = assembleTransformation (ops, i - 1, j)
                Array.append seq [| ops[i][j] |]
            | Operation.Insert _ ->
                let seq = assembleTransformation (ops, i , j - 1)
                Array.append seq [| ops[i][j] |]

