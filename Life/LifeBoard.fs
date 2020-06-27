module LifeBoard


type CellContents = 
    | FullCell
    | EmptyCell
type CellInfo = (CellContents * CellContents) * int
type LifeArray = array<CellInfo>
type BoardDimensions = {rows: int; columns: int}

type SurfaceType = 
    | Edged //cells on the edge of the board have less than 8 neighbours
    | Edgeless  // all cells have 8 neighbours

type LifeBoard =
    {   
        dimensions: BoardDimensions        
        lifeArray: LifeArray        
        changeFlag: bool // so that we can stop the timer if there are no changes 
        surfaceType: SurfaceType
    }

type Cartesian = int * int

let  CellBirth = (EmptyCell, FullCell)
let CellDeath =  (FullCell, EmptyCell)
let CellSurvival =  (FullCell, FullCell)
let CellDesert = (EmptyCell, EmptyCell)
let iter (f: unit -> unit) b = if b then f() else ()    

//let rows = 5
//let columns = 5

let CartesianToIndex(((x, y) : Cartesian, dimensions: BoardDimensions)) =     
    y * (dimensions.rows - 1) + x    

let IndexToCartesian(index: int, dimensions: BoardDimensions) : Cartesian =
    let x = index % dimensions.rows
    let y = (int) index / dimensions.rows
    (x, y)

    
let evolutionRules(kinCount, currentState) =     
    if kinCount < 2 then  //less than two kins
        EmptyCell
    else if kinCount = 3 then //three kins
        FullCell
    else if kinCount = 2 then //two kins        
        currentState
    else 
        EmptyCell   // more than three kins
    
let stringifyIntSeq (s: string) (intSeq: seq<int>)= 
    let strSeq = intSeq |> Seq.map(fun i -> i.ToString())
    sprintf "%s" (String.concat s strSeq)

let constant x = fun _ -> x

let getEdgedBounds(index, rows, columns) =   //edged bounds means that cell 0 has only 3 neighbours(kin)  
    let currentRow = int (index / columns)
    
    let IsInCurrentRowBounds = 
        fun index -> index >= (currentRow * columns) && index <= (((currentRow + 1) * columns ) - 1)

    let IsInPreviousRowBounds = 
        if (currentRow + 1) < rows then 
            fun(index) -> index >= ((currentRow + 1) * columns) && index <= (((currentRow + 2) * columns) - 1)
        else 
            constant false

    let IsInNextRowBounds =        
        if currentRow > 0 then 
            fun(index) -> index >= ((currentRow - 1) * columns) && index <= ((currentRow * columns) - 1)            
        else 
            constant false
    
    (IsInPreviousRowBounds, IsInCurrentRowBounds, IsInNextRowBounds)


let getKin rows columns surfaceType index : seq<int>= 
    
    match surfaceType with
        | Edged -> 
            let (IsInPreviousRowBounds, IsInCurrentRowBounds, IsInNextRowBounds) = getEdgedBounds(index, rows, columns)
            seq {
                for x:int in [-1; 0; 1] do 
                    let kindex = index - columns + x //row below
                    if IsInNextRowBounds (kindex) then
                        yield kindex 

                for x in [-1; 1] do 
                    let kindex = index + x //current row
                    if IsInCurrentRowBounds (kindex) then
                        yield kindex 

                for x in [-1; 0; 1] do 
                    let kindex = index + columns + x //row above
                    if IsInPreviousRowBounds (kindex) then      
                        yield kindex 
            }
            
        | Edgeless ->  
            let cellCount = rows * columns
            let indexBelow = index - columns
            let indexAbove = index + columns      
            let topRow = int (cellCount / columns) - 1
            let currentRow = int (index / columns)
            let rowBelow = 
                if currentRow > 0 then
                    currentRow - 1
                else
                    topRow
            let rowAbove = 
                if currentRow = topRow then
                    0
                else
                    currentRow + 1
            let currentColumn = index % columns
            let columnLeft = 
                if currentColumn > 0 then
                    currentColumn - 1
                else 
                    columns - 1
            let columnRight = 
                if currentColumn < (columns - 1) then 
                    currentColumn + 1
                else
                    0
                    
            let s = seq {
                yield (rowAbove * columns) + columnLeft
                yield (rowAbove * columns) + currentColumn
                yield (rowAbove * columns) + columnRight
                yield (currentRow * columns) + columnLeft
                yield (currentRow * columns) + columnRight
                yield (rowBelow * columns) + columnLeft
                yield (rowBelow * columns) + currentColumn
                yield (rowBelow * columns) + columnRight                                   
            }
            //printfn "Edgeless seq%s" (stringifyIntSeq ", " s)
            s

let updateKinCount(lifeBoard: LifeBoard, index, cellType, fGetKin) = 
    let n = 
        match cellType with
            | FullCell -> 1
            | EmptyCell -> -1
    
    let s = fGetKin index        
        
    //printfn "updateKinCount:%s" (stringifyIntSeq ", " s)
    s |>  Seq.iter(fun cellIndex ->     
        let (kinState, kinCount) = lifeBoard.lifeArray.[cellIndex]
        lifeBoard.lifeArray.[cellIndex] <- (kinState, kinCount + n)
    )

let getKinFunc(lifeBoard: LifeBoard) = 
    let rows = lifeBoard.dimensions.rows 
    let columns = lifeBoard.dimensions.columns    
    getKin rows columns lifeBoard.surfaceType
    

let updateCellStates(lifeBoard: LifeBoard) = 
    let kinGet = getKinFunc(lifeBoard)  // this function could be stored on the board

    let ((_, changes), kList) = 
        lifeBoard.lifeArray |>     
        Array.fold(fun ((index, changeCount), kinList) ((_, currentState), kinCount)->
            let newState = evolutionRules(kinCount, currentState)
            let (newKinList, change) = 
                match newState <> currentState with 
                | true ->                                           
                    lifeBoard.lifeArray.[index] <- ((currentState, newState), kinCount)                            
                    (index, newState) :: kinList, 1
                | false -> kinList, 0

            ((index + 1, changeCount + change), newKinList)
        ) ((0, 0), List.empty<int * CellContents>)

    kList |> List.iter(fun (index, newState) -> updateKinCount(lifeBoard, index, newState, kinGet))
    changes


let initialLifeBoard( dimensions: BoardDimensions) = 
    let cellCount = dimensions.rows * dimensions.columns
    let random = new System.Random()
    let nextRandom () = random.Next(0, cellCount - 1)
    let randomlist = [ for i in 1 .. cellCount -> nextRandom ()]
    let arr = 
        List.map(fun n -> 
            match (n % 5) with
                | 0 -> CellBirth, 0
                | _ -> CellDesert, 0
            ) randomlist
    let lifeArray = Array.ofList arr
    let lifeBoard = {lifeArray = lifeArray; dimensions = {rows = dimensions.rows; columns = dimensions.columns}; changeFlag = true; surfaceType = Edgeless}
    let kinGet = getKinFunc(lifeBoard)

    lifeArray |> Array.fold(fun index (cellState, _kinCount)->
        (cellState = CellBirth) |> iter  (fun() ->
            updateKinCount(lifeBoard, index, FullCell, kinGet)            
        )                
        index + 1
    ) 0 |> ignore
    
    lifeBoard
    


let renderBoard(lifeBoard: LifeBoard) =         
    lifeBoard.lifeArray |> Array.fold(fun index ((prevState, currentState), kinCount)->
         (currentState = FullCell) |> iter  (fun() ->
             printfn "Cell index %d: is Full" index
         )                
         index + 1
     ) 0 |> ignore

// program steps
    // 1) intitialise board
    // 2) render board
    // update states
    // Go to 1 (render)
let evolve(lifeBoard:LifeBoard) = 
    let changes = updateCellStates(lifeBoard)
    {lifeBoard with changeFlag = (changes > 0);} 

let contrivedLifeBoard(dimensions: BoardDimensions) = 
    let cellCount = dimensions.rows * dimensions.columns
    
    let arr = 
        seq { 
            for index in 0 .. (cellCount - 1) do
                let i = index % dimensions.columns 
                if i = 0 || i = (dimensions.columns  - 1) then
                    yield CellBirth, 0
                else 
                    yield CellDesert, 0
        }
    let lifeArray = Array.ofSeq arr
    let lifeBoard = {lifeArray = lifeArray; dimensions = {rows = dimensions.rows; columns = dimensions.columns}; changeFlag = true; surfaceType = Edgeless}
    let kinGet = getKinFunc(lifeBoard)
    
    lifeArray |> Array.fold(fun index (cellState, _kinCount)->
        (cellState = CellBirth) |> iter  (fun() ->
            updateKinCount(lifeBoard, index, FullCell, kinGet)            
        )                
        index + 1
    ) 0 |> ignore
    
    lifeBoard
    

let f() = 
    //let lifeBoard:LifeBoard = initialLifeBoard({rows = 5; columns = 5})
    let lifeBoard:LifeBoard = contrivedLifeBoard({rows = 5; columns = 5})
    renderBoard(lifeBoard)
    let c = updateCellStates(lifeBoard)
    let lifeBoard = {lifeBoard with changeFlag = (c > 0)}    
    renderBoard(lifeBoard)
    let kinGet = getKinFunc(lifeBoard)
    

    updateKinCount(lifeBoard, 0, FullCell, kinGet)
