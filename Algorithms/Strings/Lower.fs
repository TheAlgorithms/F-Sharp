namespace Algorithms.Strings

module Lower =
    /// <summary>
    /// Will convert the entire string to lowercase letters
    /// </summary>
    /// <example>
    /// <code>
    /// lower("wow")
    /// 'wow'
    /// 
    /// lower("HellZo")
    /// hellzo'
    /// 
    /// lower("WHAT")
    /// 'what'
    /// 
    /// lower("wh[]32")
    /// 'wh[]32'
    /// 
    /// lower("whAT")
    /// 'what'
    /// </code>
    /// </example>
    /// <param name="word"></param>
    /// <returns></returns>
    let lower(word:string):string =
        let mutable str = ""
        // converting to ascii value int value and checking to see if char is a capital
        for letter in word do
                // letter if it is a capital letter it is getting shift by 32 which makes it a lower case letter
                if letter >= 'A' && letter <= 'Z'
                then str <- str + (string) ((char) ((int) letter + 32))
                else str <- str + (string) letter
        str