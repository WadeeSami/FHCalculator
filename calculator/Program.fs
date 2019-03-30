open System
open System.Text.RegularExpressions

let defaultDelimmiter = ";"
let (|SingleDigitString|_|) input =
   let m = Regex.Match(input, @"^[0-9]+$")
   if (m.Success) then Some input else None

let (|TwoDigitString|_|) input =
   let m = Regex.Match(input, @"^([0-9]+(,|\n))+[0-9]+$")
   if (m.Success) then Some input else None

let (|AdvancedString|_|) input =
   let delimiters = [ ';'; ',' ]
   let m = Regex.Match(input, @"^//(\[(?<delim>.+)\])+\n(-?[0-9]+.+)+-?[0-9]+$")
   if (m.Success) then Some(input.Split '\n') else None

let getNumbersPart (originalString : String) =
    originalString.Split('\n').[1]

let getDelimitersPart (splitString : String []) =
    // gets the delimiter after splitting original string with \n
    if splitString.Length > 1 then
        let delimStr = splitString.[0].Split([| "//" |], StringSplitOptions.RemoveEmptyEntries).[0]
        let sliced = delimStr.[1..delimStr.Length-2]
        sliced.Split([|"]["|], StringSplitOptions.RemoveEmptyEntries) 
    else
        [|defaultDelimmiter|]

let add s =
    match s with
    | "" ->
        0
    | SingleDigitString x ->
        int x
    | TwoDigitString x ->
        x.Split([| ','; '\n' |]) |> Seq.map int |> Seq.sum
    | AdvancedString x ->
        let nums = getNumbersPart s
        let delims = getDelimitersPart x
        let arrayOfNums = nums.Split(delims, StringSplitOptions.RemoveEmptyEntries) |> Seq.map int
        let negatives = arrayOfNums |> Seq.filter (fun x -> int x < 0)
        if Seq.length negatives > 0 then
            let errStatement = sprintf "Operation Not Allowed On Negative Numbers %A" (negatives |> Seq.map string |> String.concat ",")
            failwith errStatement

        else
            arrayOfNums |>  Seq.filter (fun x -> x < 1000) |> Seq.sum
    | _ ->
        failwith "Invalid Input"

printfn "%i" (add "//[,,][*]\n12,,3,,1000,,100*200")
printfn "%i" (add "//[%][***]\n1***2%34")
//printfn "%i" (add "//[%][***]\n1***2%-34")
printfn "%i" (add "//[,,]\n1200,,3000,,1000,,10000,,2000")
