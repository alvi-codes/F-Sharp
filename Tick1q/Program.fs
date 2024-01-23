open System

//------------------------write your answer function(s) here---------------------//

// top-level subfunctions of polarToCartesianApprox (if any)

/// answer to Tick1
// the header given here is correct.
let polarToCartesianApprox (r, theta) n =
    let n_cos = n / 2               /////
    let n_sin = (n-1) / 2           /////

    let factorial m =
        if m = 0 then 1.0
        else [1.0 .. float m] |> List.reduce (*)

    let sinThetaTerm k =
        if n = 0 then 0.0           /////
        else (((-1.0) ** float k) * (theta ** (2.0 * float k + 1.0))) / (factorial (2 * k + 1))

    let cosThetaTerm k =
        (((-1.0) ** float k) * (theta ** (2.0 * float k))) / (factorial (2 * k))

    let sin_theta =
        // [0 .. n_sin] |> List.map sinThetaTerm |> List.reduce (+)
        List.reduce (+) (List.map sinThetaTerm [0 .. n_sin])

    let cos_theta =
        [0 .. n_cos] |> List.map cosThetaTerm |> List.reduce (+)

    (r * cos_theta, r * sin_theta)

    // COMMENTS for Professor:
    // Assume I remove the lines marked with ///// on their right.
    // Hi Sir,
    // I am really not sure why I am unable to pass the test cases. 
    // I know my factorial function is working properly, and the overall
    // polarToCartesianApprox function seems to make sense as well. But, I believe it is 
    // the sin_theta and cos_theta computation that is sometimes very close to 
    // the actual answer, but does not exactly match the floating point 
    // values. Also, some values are very far off. But it seems like I have
    // put down the formula correctly though.
    // Thank you in advance for your feedback.




//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [1.;2.] [1.;2.]
    List.allPairs testPolarCoords [0;1;2;3;10]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [
        ((1.0, 1.0), 0, (1.0, 0.0))       
        ((1.0, 2.0), 0, (1.0, 0.0))        
        ((2.0, 1.0), 0, (2.0, 0.0))        
        ((2.0, 2.0), 0, (2.0, 0.0))        
        ((1.0, 1.0), 1, (1.0, 1.0))        
        ((1.0, 2.0), 1, (1.0, 2.0))        
        ((2.0, 1.0), 1, (2.0, 2.0))        
        ((2.0, 2.0), 1, (2.0, 4.0))        
        ((1.0, 1.0), 2, (0.5, 1.0))        
        ((1.0, 2.0), 2, (-1.0, 2.0))        
        ((2.0, 1.0), 2, (1.0, 2.0))        
        ((2.0, 2.0), 2, (-2.0, 4.0))        
        ((1.0, 1.0), 3, (0.5, 0.8333333333))        
        ((1.0, 2.0), 3, (-1.0, 0.6666666667))        
        ((2.0, 1.0), 3, (1.0, 1.666666667))        
        ((2.0, 2.0), 3, (-2.0, 1.333333333))        
        ((1.0, 1.0), 10, (0.5403023038, 0.8414710097))        
        ((1.0, 2.0), 10, (-0.4161552028, 0.9093474427))        
        ((2.0, 1.0), 10, (1.080604608, 1.682942019))        
        ((2.0, 2.0), 10, (-0.8323104056, 1.818694885))
    ]
/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001
    let testItem fn (coords, n, (expectedX,expectedY) as expected) =
        let actualX,actualY as actual = testFun coords n
        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A"coords n expected actual
            1
        else
            0
    printfn "Starting tests..."
    let numErrors = List.sumBy (testItem testFun) testData
    printfn "%d tests Passed %d tests failed." (testData.Length - numErrors) numErrors

[<EntryPoint>]
let main argv =
    testBench testBenchData polarToCartesianApprox
    0 // return an integer exit code
