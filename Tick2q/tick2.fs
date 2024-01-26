module Tick2

//---------------------------Tick2 PartA skeleton code-------------------------------//


module PartACase1 =
    () // dummy value to make submodule non-empty
    // Three record types, one data value of each type. Choose suitable names.
    
    type mScGrade = {
        Distinction: int;
        Merit:       int;
        Pass:        int;
        Fail:        int;
    }

    let mScGradeBoundaries = {
        Distinction= 70;
        Merit=       60;
        Pass=        50;
        Fail=         0;
    }

    type mEngGrade = {
        First:       int;
        UpperSecond: int;
        LowerSecond: int;
        Fail:        int;
    }

    let mEngGradeBoundaries = {
        First=       70;
        UpperSecond= 60;
        LowerSecond= 50;
        Fail=         0;
    }

    type bEngGrade = {
        First:       int;
        UpperSecond: int;
        LowerSecond: int;
        Third:       int;
        Fail:        int;
    }

    let bEngGradeBoundaries = {
        First=       70;
        UpperSecond= 60;
        LowerSecond= 50;
        Third=       40;
        Fail=         0;
    }


module PartACase2 =
    () // dummy value to make submodule non-empty
    // One record type, three data values of this type. Choose suitable names.
    type gradeBoundaries = {
        Bound70: string;
        Bound60: string;
        Bound50: string;
        Bound40: string;
        Bound0:  string;
    }

    let mScGradeBoundaries = {
        Bound70= "Distinction";
        Bound60= "Merit";
        Bound50= "Pass";
        Bound40= "";
        Bound0=  "Fail";
    }

    let mEngGradeBoundaries = {
        Bound70= "First";
        Bound60= "UpperSecond";
        Bound50= "LowerSecond";
        Bound40= "";
        Bound0=  "Fail";
    }
 
    let bEngGradeBoundaries = {
        Bound70= "First";
        Bound60= "UpperSecond";
        Bound50= "LowerSecond";
        Bound40= "Third";
        Bound0=  "Fail";
    }


module PartACase3 =
    () // dummy value to make submodule non-empty
    // One type, three data values of this type. Choose suitable names.
    let mScGradeBoundaries = ["Distinction",70; "Merit",60; "Pass",50; "Fail",0]

    let mEngGradeBoundaries = ["First",70; "UpperSecond", 60; "LowerSecond",50; "Fail",0] 

    let bEngGradeBoundaries = ["First",70; "UpperSecond", 60; "LowerSecond",50; "Third",40; "Fail",0] 

//---------------------------Tick2 PartB case 2 skeleton code-------------------------------//

module PartBCase2 =

    open PartACase2

    let findBoundaries (course: string) : Option<gradeBoundaries> =
        match course with
        | "MSc" -> Some mScGradeBoundaries
        | "MEng" -> Some mEngGradeBoundaries
        | "BEng" -> Some bEngGradeBoundaries
        | _ -> None

    let classify (course: string) (mark: float) : Result<string, string> =
        // Check if the mark is in the valid range
        if mark < 0.0 || mark > 100.0 then
            Error "Invalid mark. Marks must be in the range 0 - 100."
        else
            // Determine the grade boundaries based on the course
            match findBoundaries course with
            | Some boundaries ->
                // Determine the classification based on the mark
                let classification =
                    if (course = "MEng" || course = "MSc") && (mark < 50.0) then boundaries.Bound0
                    else 
                        if mark   >= 70.0 then boundaries.Bound70
                        elif mark >= 60.0 then boundaries.Bound60
                        elif mark >= 50.0 then boundaries.Bound50
                        elif mark >= 40.0 then boundaries.Bound40
                        else boundaries.Bound0

                Ok classification
            | None -> Error "Invalid course. Supported courses are MSc, MEng, and BEng."

module OLD_PartBCase2 =

    open PartACase2 // get unqualified access to Case 2 types and values

    /// Return as a Ok string the name of the correct classification for a student
    /// on given course with given mark.
    /// Return Error if course or mark are not possible (marks must be in range 100 - 0). 
    /// The error message should say what the problem in the data was.
    let classify (course: string) (mark: float) : Result<string,string> =
        printf "Run \n"
        // Check if the mark is in the valid range
        if mark < 0.0 || mark > 100.0 then
            Error "Invalid mark. Marks must be in the range 0 - 100."
        else
            // Determine the grade boundaries based on the course
            let boundaries =
                match course with
                | "MSc" -> mScGradeBoundaries
                | "MEng" -> mEngGradeBoundaries
                | "BEng" -> bEngGradeBoundaries
                | _ -> failwithf "Invalid course. Supported courses are MSc, MEng, and BEng."

            // Determine the classification based on the mark
            let classification =
                if (course = "MEng" || course = "MSc") && (mark < 50.0) then boundaries.Bound0
                else 
                    if mark   >= 70.0 then boundaries.Bound70
                    elif mark >= 60.0 then boundaries.Bound60
                    elif mark >= 50.0 then boundaries.Bound50
                    elif mark >= 40.0 then boundaries.Bound40
                    else boundaries.Bound0

            Ok (classification)

//---------------------------Tick2 PartB case 3 skeleton code-------------------------------//
module PartBCase3 =

    open PartACase3 // get unqualified access to Case 3 types and values

    /// Return as a Ok string the name of the correct classification for a studen on given course with given mark.
    /// Return Error if course or mark are not possible (marks must be in range 100 - 0). The error message should say what the problem in the data was.
 
    // Step 1: Use the course to match and find the specific list
    let getGradeBoundaries (course: string) =
        match course with
        | "MSc" -> mScGradeBoundaries
        | "MEng" -> mEngGradeBoundaries
        | "BEng" -> bEngGradeBoundaries
        | _ -> []

    // Step 2: Swap the pair values
    let swapPairValues (lst: ('a * 'b) list) : ('b * 'a) list =
        lst |> List.map (fun (x, y) -> (y, x))

    // Step 3: Make it a map using Map.fromList
    let makeMap (lst: ('a * 'b) list) : Map<'a, 'b> =
        Map.ofList lst

    // Step 4: Use another function to find the grade boundary
    let findGradeBoundary (course: string) (mark: float) : (int) =
            if (course = "MEng" || course = "MSc") && (mark < 50.0) then 0
            else 
                if mark   >= 70.0 then 70
                elif mark >= 60.0 then 60
                elif mark >= 50.0 then 50
                elif mark >= 40.0 then 40
                else 0

    // Main classify function
    let classify (course: string) (mark: float) : Result<string, string> =
        // Check if the mark is in the valid range
        if mark < 0.0 || mark > 100.0 then
            Error "Invalid mark. Marks must be in the range 0 - 100."
        else
            // Step 1: Get the grade boundaries for the course
            let boundaries = getGradeBoundaries course

            // Step 2: Swap the pair values
            let swappedBoundaries = swapPairValues boundaries

            // Step 3: Make it a map using Map.fromList
            let boundariesMap = makeMap swappedBoundaries

            // Step 4: Use another function to find the grade boundary
            let boundry = findGradeBoundary course mark

            Ok (boundariesMap[boundry])

//------------------------------------Tick2 PartC skeleton code-----------------------------------//

module PartC =
    open PartACase3 // get unqualified access to Case 3 types and values
    open PartBCase3 // get unqualified access to classify function

    type Marks = {Mark1: float} // simplified set of marks (just one mark) used for compilation of code

    /// Return the total mark for a student used to determine classification. 
    /// marks:  constituent marks of student on given course.
    /// course: name of course student is on
    /// Return None if the course is not valid or any of the marks are
    /// outside the correct range 0 - 100.
    let markTotal (marks: Marks) (course: string) : float option =
        match course with
        | "MEng"  | "BEng" | "MSc" when marks.Mark1 <= 100.0 && marks.Mark1 >= 0.0 ->
            Some marks.Mark1 // in this case with only one mark, student total is just the mark!
        | _ -> None

    /// Operation:
    /// 1. Return an error if boundary is not a valid boundary for course.
    /// 2. Return IsAboveBoundary = true if total is above or equal to boundary
    /// 3. Return Uplift = Some uplift if total is in the valid possible uplift range (0 - -2.5%) of boundary.
    let upliftFunc 
        (marks: Marks) 
        (boundary:string) 
        (course: string)
            : Result<{|IsAboveBoundary: bool; Uplift:float option|}, string> =
        // Use markTotal to calculate total from marks
        // Also return an error if markTotal fails to calculate a mark
        // Ok return type is an anonymous record see link in WS2.
        // upliftFunc is assumed (when implemented) to take boundary info from a value defined above
        // with whatever data structure is used for it. In Part C you do not implement
        // upliftFunc and so need not consider any of this.
        failwithf "Not Implemented" // do not change - implementation not required

    /// Given a list of boundaries, and a course, and a student's marks:
    /// Return the student classification, or an error message if there is
    /// any error in the data.
    /// boundaries: name only, subfunctions will know boundary marks based on course, 
    /// this function needs only the results of calling its subfunctions.
    let classifyAndUplift 
        (boundaries: string list)
        (course: string) 
        (marks: Marks)
                : Result<string,string> =
        // Use upliftFunc and markTotal and classify.
        // Assume that the student can be within possible uplift range of at most one boundary.
        // Assume that classify is correct unless student is within uplift range of a given boundary,
        // If student is within uplift range of a boundary `boundaryName` work out classification as:
            // let total = markTotal marks course
            // let effectiveMark = total + upliftFunc marks boundaryName course
            // let className = classify course effectiveMark
            // Return Ok classname or an error if there is any error.
            // (option and error returns ignored in above comments, must be dealt with)

        failwithf "Not implemented" // replace by your code ()





//------------------------------Simple test data and functions---------------------------------//
module TestClassify =
    /// test data comaptible with the Tick 2 problem
    let classifyUnitTests = [
        "MEng",75.0, Ok "First"
        "MSc", 75.0,Ok "Distinction"
        "BEng", 75.0, Ok "First"
        "MEng",65.0, Ok "UpperSecond"
        "MSc", 65.0, Ok "Merit"
        "BEng", 65.0, Ok "UpperSecond"        
        "MEng",55.0, Ok "LowerSecond"
        "MSc", 55.0, Ok "Pass"
        "BEng", 55.0, Ok "LowerSecond"        
        "MEng",45.0, Ok "Fail"
        "MSc", 45.0, Ok "Fail"
        "BEng", 45.0, Ok "Third"
        "BEng", 35.0, Ok "Fail"        
    ]

    let runClassifyTests unitTests classify testName =
        unitTests
        |> List.map (fun (data as (course,mark,_)) -> classify course mark, data)
        |> List.filter (fun (actualClass, (_,_,className)) -> actualClass <> className)
        |> function 
            | [] -> printfn $"all '{testName}' tests passed."
            | fails -> 
                fails 
                |> List.iter (fun (actual, (course,mark,className)) 
                                -> printfn $"Test Failed: {course}, {mark}, expected className={className}, \
                                          actual className={actual}")


//-------------------------------------------------------------------------------------------//
//---------------------------------Run Part B tests------------------------------------------//
//-------------------------------------------------------------------------------------------//

open TestClassify
let runTests() =
    runClassifyTests classifyUnitTests PartBCase2.classify "Case2"
    runClassifyTests classifyUnitTests PartBCase3.classify "Case3"


//-------------------------------------------------------------------------------------------//
//---------------------------------Tick2 Part X Skeleton code--------------------------------//
//-------------------------------------------------------------------------------------------//
module PartX =
    type Lens<'A,'B> = ('A -> 'B) * ('B -> 'A -> 'A)

    let lensMap (lens: Lens<'A,'B>) (f: 'B -> 'B) (a: 'A) =       
        (fst lens a |> f |> snd lens) a

    let mapCAndB (lensC: Lens<'A,'C>) (lensB: Lens<'A,'B>) (fc:'C->'C) (fb: 'B->'B) =
        lensMap lensC fc >> lensMap lensB fb

    let combineLens (l1: Lens<'A, 'B>) (l2: Lens<'B, 'C>) : Lens<'A, 'C> =
        let getterAC a =
            let b = fst l1 a
            fst l2 b

        let setterAC c a =
            let (bGetter, bSetter) = l1
            let (cGetter, cSetter) = l2
            let b = bGetter a
            let updatedB = cSetter c b
            bSetter updatedB a

        (getterAC, setterAC)