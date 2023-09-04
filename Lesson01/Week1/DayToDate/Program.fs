// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F# date to day"



//part one
//2) 
let isLeap year = 
    if year % 4 = 0 then
        if year % 100 = 0 then
            year % 400 = 0
        else
            true
    else
        false

let isLeapMatch year =
    match year with
    | year when year % 400 = 0 -> true
    | year when year % 100 = 0 -> false
    | year when year % 4 = 0 -> true
    | _ -> false

let LeapTest1 = isLeapMatch 1992 
let LeapTest2 = isLeapMatch 2000
let LeapTest3 = isLeapMatch 1900

// 3)

let daysInYear year =
    if isLeapMatch year then 366
    else 365

let rec daysToEndYear year =
    if year = 1970 then 365
    else daysToEndYear (year-1) + daysInYear year
    
    
    // optional Handle years before 1970 
    

let Test1971 = daysToEndYear 1971

let Test1978 = daysToEndYear 1978


// 4)

let correction (m: int, y: int) =
    if m = 1 then 0
    else
        if isLeap y
            then 1
        else
            2
    
let daysToEndMonth(m: int, y: int) =
    (367*m+5)/12-correction(m,y) + daysToEndYear y - daysInYear y
    
   
let Test91970 = daysToEndMonth (9,1970)
let Test61989 = daysToEndMonth (6,1989)




//5)
let date = (1,1,1970)
 
let epochDay(day: int, month: int, year: int ) =
    if(month = 1) then
        if year = 1970 then day - 1
        else 
         daysToEndYear(year-1) + day - 1
    else
    daysToEndMonth(month-1,year) + day - 1
    
let TestDay = epochDay date
let TestDay2 = epochDay(4,9,2023)
        