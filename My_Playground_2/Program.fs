    open System
    open Expecto.ExpectoFsCheck
    open FsCheck
    
    let revOfRevIsOrig (x: float list) = 
        List.rev (List.rev x) = x

    let plusIsAssociative a b c = 
       (a + b) + c = a + (b + c)
    
    let ``de Morgan's Theorem Fails`` a b = 
        (a && b) = not ((not a) || (not b))


    [<EntryPoint>]
    let main argv =
        printfn "Testing with FSCheck"

        // Check.Verbose revOfRevIsOrig |> ignore
        // Check.Quick plusIsAssociative
        Check.Quick ``de Morgan's Theorem Fails``


        Console.ReadKey() |> ignore // wait at end
        0 // return an integer exit code
    