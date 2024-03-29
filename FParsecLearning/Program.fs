﻿open FParsec

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let test p str =
    match run p str with
    | Success(result, userState, opt3) -> printfn "Success : %A ; %A ; %A" result userState opt3
    | Failure(errorMsg, parserError ,userState) -> printfn "Failure : %A" errorMsg

type ExpressionTerm =
    | Array of float list
    | Number of double

let pNumericTerm =
    pfloat
    |>> (fun a -> Number a)

let pArray =
    (pchar '[') >>. (spaces) >>. (pfloat .>>. (many ((spaces) >>. (pchar ';') >>. spaces >>. pfloat))) .>> (opt (pchar ';')) .>> (spaces) .>> (pchar ']')
    |>> (fun (first, rest) ->
            Array (first :: rest))


[<EntryPoint>]
let main argv = 
    test pfloat "1.56E3"
    test pfloat "1.56E 3"
    test pfloat "1.a56E3"
    test pArray "[ 1;2.5;3;4]"
    test (anyOf ['a'..'z']) "1"
    test (pArray <|> pNumericTerm) "1"
    
    printfn "ABD with no attempts:"
    //Notice how there is still error
    test (((pchar 'A' .>>. pchar 'B' .>>. pchar 'C')) <|> ((pchar 'A' .>>. pchar 'B' .>>. pchar 'D'))) "ABD"

    printfn "ABE with two attempts:"
    //Notice how the error is pointing to D
    test ((attempt (pchar 'A' .>>. pchar 'B' .>>. pchar 'C')) <|> (attempt (pchar 'A' .>>. pchar 'B' .>>. pchar 'D'))) "ABE"

    printfn "ABE with backtracking andThen"
    //Notice how the error is pointing to D now
    test (((pchar 'A' .>>. pchar 'B' .>>.? pchar 'C')) <|> ((pchar 'A' .>>. pchar 'B' .>>. pchar 'D'))) "ABE"

    //Understanding backtracking
    let bInBrackets = pstring "[" >>. pstring "b" .>> pstring "]"
    let ac = pstring "a" .>>. pstring "c"
    let desiredParser = (pstring "a" .>>.? bInBrackets) <|> (ac)
    test desiredParser "a[B]"
    test desiredParser "a[b]"
    test desiredParser "ac"
    test desiredParser "ab"


    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
