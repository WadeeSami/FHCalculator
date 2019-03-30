open System
open System.Text.RegularExpressions

let (|SingleDigitString|_|) input =
   let m = Regex.Match(input, @"^[0-9]+$")
   if (m.Success) then Some input else None

let (|TwoDigitString|_|) input =
   let m = Regex.Match(input, @"^([0-9]+,)+[0-9]+$")
   if (m.Success) then Some input else None

let rec add s =
    match s with
    | "" ->
        0
    | SingleDigitString x ->
        int x
    | TwoDigitString x ->
        x.Split ',' |> Seq.map int |> Seq.sum
    | _ ->
        failwith "Invalid Input"

printfn "%i" (add "11,22,33,1000")
