open System
open System.Text.RegularExpressions

let defaultDelimmiter = [|","; "\n"|]
let (|SingleDigitString|_|) input =
   let m = Regex.Match(input, @"^[0-9]+$")
   if (m.Success) then Some input else None

let (|TwoDigitString|_|) input =
   let m = Regex.Match(input, @"^([0-9]+(,|\n))+[0-9]+$")
   if (m.Success) then Some input else None

let (|AdvancedString|_|) input =
   let m = Regex.Match(input, @"^//(\[(?<delim>.+)\])+\n(-?[0-9]+(.+|\n))+-?[0-9]+$")
   if (m.Success) then Some(input) else None

let getNumbersPart (originalString : String) =
    let indexOfJoint = originalString.IndexOf("\n")
    originalString.[indexOfJoint+1..]

let getDelimitersPart (originalString : String) =
    let delimsNewLineIndex = originalString.IndexOf("\n")
    let mutable delimsPart = originalString.[..delimsNewLineIndex-1]
    
    delimsPart <- delimsPart.Split([| "//" |], StringSplitOptions.RemoveEmptyEntries).[0]
    let sliced = delimsPart.[1..delimsPart.Length-2]
    sliced.Split([|"]["|], StringSplitOptions.RemoveEmptyEntries) |> Array.append defaultDelimmiter


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
        let delims = getDelimitersPart s
        let arrayOfNums = nums.Split(delims, StringSplitOptions.RemoveEmptyEntries) |> Seq.map int
        let negatives = arrayOfNums |> Seq.filter (fun x -> int x < 0)
        if Seq.length negatives > 0 then
            let errStatement = sprintf "Operation Not Allowed On Negative Numbers %A" (negatives |> Seq.map string |> String.concat ",")
            failwith errStatement

        else
            arrayOfNums |>  Seq.filter (fun x -> x < 1000) |> Seq.sum
    | _ ->
        failwith "Invalid Input"
    

printfn "%i" (add "100")
printfn "%i" (add "10,20")
printfn "%i" (add "10,20\n30")
printfn "%i" (add "//[,,][*]\n12,,3,,1000,,100*200,,5\n8")
printfn "%i" (add "//[,,][*]\n12,,3,,1000,,100*200,,5\n8,100")
printfn "%i" (add "//[%][***]\n1***2%34,100%300")
printfn "%i" (add "//[,,]\n1200,,3000,,1000,,10000,,2000\n20")
printfn "%i" (add "//[%][***]\n1***2%-34")
