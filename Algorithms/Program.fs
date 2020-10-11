open Sort

[<EntryPoint>]
let main argv =
    // For testing
    let list = [| 2; 45; 59; 1; 0; -2; 5; 19 |]
    for item in CombSort.Sort list do
        System.Console.WriteLine item
    0
