open FParsec

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let test p str =
    match run p str with
    | Success(result, remaining, opt3) -> printfn "Success : %A ; %A ; %A" result remaining opt3
    | Failure(errorMsg, _ , _) -> printfn "Failure : %A" errorMsg


[<EntryPoint>]
let main argv = 
    test pfloat "1.56E3"
    test pfloat "1.56E 3"
    test pfloat "1.a56E3"

    test ((attempt (pchar 'A' .>>. pchar 'B' .>>. pchar 'C')) <|> (attempt (pchar 'A' .>>. pchar 'B' .>>. pchar 'D'))) "ABE"

    test (((pchar 'A' .>>. pchar 'B' .>>.? pchar 'C')) <|> ((pchar 'A' .>>. pchar 'B' .>>. pchar 'D'))) "ABE"

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
