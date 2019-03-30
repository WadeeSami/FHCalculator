open System
open System.Text.RegularExpressions

let supportedDelimiters = [ ','; '\n' ]
let defaultDelimmiter = ";"
let (|SingleDigitString|_|) input =
   let m = Regex.Match(input, @"^[0-9]+$")
   if (m.Success) then Some input else None

let (|TwoDigitString|_|) input =
   let m = Regex.Match(input, @"^([0-9]+(,|\n))+[0-9]+$")
   if (m.Success) then Some input else None

let (|AdvancedString|_|) input =
   let delimiters = [ ';'; ',' ]
   let m = Regex.Match(input, @"^//(?<delim>.+)\n([0-9]+\k<delim>)+[0-9]+$")
   if (m.Success) then Some(input.Split '\n') else None

let getNumbersPart (originalString : String) =
    originalString.Split('\n').[1]

let getDelimitersPart (splitString : String []) =
    if splitString.Length > 1 then
        splitString.[0].Split([| "//" |], StringSplitOptions.RemoveEmptyEntries).[0]
    else
        defaultDelimmiter

let rec add s =
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
        nums.Split([| delims |], StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Seq.sum
    | _ ->
        failwith "Invalid Input"
        
printfn "%i" (add "//,,\n12,,3,,1000")
