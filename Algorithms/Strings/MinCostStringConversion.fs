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
            Array.init (source.Length + 1) (fun  _ -> Array.init (destination.Length + 1) (fun _ -> None))

        let ops =
            Array.init (source.Length + 1) (fun  _ -> Array.init (destination.Length + 1) (fun _ -> None))

        costs.[0].[0] <- Some 0
        ops.[0].[0] <- Some (Operation.Copy 'a') // There is no operation to perform, assigning dummy operation to satisfy compiler


        for i = 1 to source.Length do
            costs.[i].[0] <- Some (i * deleteCost)
            ops.[i].[0] <- Some (Operation.Delete source.[i - 1])

        for i = 1 to destination.Length do
            costs.[0].[i] <- Some (i * insertCost)
            ops.[0].[i] <- Some (Operation.Insert destination.[i - 1])

        for i in 1 .. source.Length do
            for j in 1 .. destination.Length do
                if source.[i - 1] = destination.[j - 1] then
                    costs.[i].[j] <- Some (costs.[i - 1].[j - 1].Value + copyCost)
                    ops.[i].[j] <- Some (Operation.Copy (source.[i - 1]))
                else
                    costs.[i].[j] <- Some (costs.[i - 1].[j - 1].Value + replaceCost)
                    ops.[i].[j] <- Some (Operation.Replace (source.[i - 1], destination.[j - 1]))

                if costs.[i - 1].[j].Value + deleteCost < costs.[i].[j].Value then
                    costs.[i].[j] <- Some (costs.[i - 1].[j].Value + deleteCost)
                    ops.[i].[j] <- Some (Operation.Delete (source.[i - 1]))

                if costs.[i].[j - 1].Value + insertCost < costs.[i].[j].Value then
                    costs.[i].[j] <- Some (costs.[i].[j - 1].Value + insertCost)
                    ops.[i].[j] <- Some (Operation.Insert destination.[j - 1])

        costs |> Array.map (Array.map Option.get), ops |> Array.map (Array.map Option.get)

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

