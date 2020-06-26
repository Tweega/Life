module LifeBoard


type CellContents = 
    | FullCell
    | EmptyCell
type CellInfo = (CellContents * CellContents) * int
type LifeArray = array<CellInfo>
type BoardDimensions = {rows: int; columns: int}

type LifeBoard =
    {   
        dimensions: BoardDimensions        
        lifeArray: LifeArray        
        changeCount: int
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
    let r = 
        match (dimensions.rows < 1) with 
        | true -> 1
        | false -> dimensions.rows

    let x = index % dimensions.rows
    let y = (int) index / r
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
    

let updateKinCount(lifeBoard: LifeBoard, index, cellType) = 
    let n = 
        match cellType with
            | FullCell -> 1
            | EmptyCell -> -1
    let rows = lifeBoard.dimensions.rows 
    let columns = lifeBoard.dimensions.columns
    let cellCount = rows * columns    
    
    seq {
        for x:int in [-1; 0; 1] do yield index - columns - x//previous row            
        for x in [-1; 1] do yield index + x //current row
        for x in [-1; 0; 1] do yield index + columns + x //next row
    }
    |> Seq.filter(fun ndx -> (ndx >= 0) && (ndx < cellCount) )    
    |>  Seq.iter(fun cellIndex ->     
        let (kinState, kinCount) = lifeBoard.lifeArray.[cellIndex]
        lifeBoard.lifeArray.[cellIndex] <- (kinState, kinCount + n)
    )

let updateCellStates(lifeBoard: LifeBoard) = 
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

    kList |> List.iter(fun (index, newState) -> updateKinCount(lifeBoard, index, newState))
    changes


let initialLifeBoard( dimensions: BoardDimensions) = 
    let cellCount = dimensions.rows * dimensions.columns
    let random = new System.Random()
    let nextRandom () = random.Next(0, 99)
    let randomlist = [ for i in 1 .. cellCount -> nextRandom ()]
    let arr = 
        List.map(fun n -> 
            match (n % 5) with
                | 0 -> CellBirth, 0
                | _ -> CellDesert, 0
            ) randomlist
    let lifeArray = Array.ofList arr
    let lifeBoard = {lifeArray = lifeArray; dimensions = {rows = dimensions.rows; columns = dimensions.columns}; changeCount = 0}

    lifeArray |> Array.fold(fun index (cellState, _kinCount)->
        (cellState = CellBirth) |> iter  (fun() ->
            updateKinCount(lifeBoard, index, FullCell)            
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
    {lifeBoard with changeCount = (lifeBoard.changeCount + changes) % (lifeBoard.dimensions.rows * lifeBoard.dimensions.columns + 1)} 

let f() = 
    let lifeBoard:LifeBoard = initialLifeBoard({rows = 5; columns = 5})
    renderBoard(lifeBoard)
    let c = updateCellStates(lifeBoard)
    let lifeBoard = {lifeBoard with changeCount = (lifeBoard.changeCount + c) % (lifeBoard.dimensions.rows * lifeBoard.dimensions.columns + 1)}    
    renderBoard(lifeBoard)
    




