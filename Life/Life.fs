// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Life

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open LifeBoard

module App =
    type PlayState = 
        | LifeRunning
        | LifePaused

    type PlayMode =
        | RunMode of PlayState
        | ConfigureMode 

    
    type Model = 
      {
        lifeBoard: LifeBoard
        mode: PlayMode
        stepCount: int
        tickCount: int
        timerOn: bool
        timerInterval: int
      }
    
    type Msg = 
        //configure mode
        | ToggleCell of Cell    
        | ResetConfig        
        | SetTimerInterval of int

        // play mode
        | PauseLife
        | RunLife
        | StepLife        
        | TimedTick
        | RestartBoard
        | ToggleTimer

        // both modes
        | ToggleMode of PlayMode

    let initModel() = { lifeBoard = initialLifeBoard({rows = 10; columns = 10}); mode = ConfigureMode; stepCount = 0; tickCount = 0; timerOn = false; timerInterval = 1}

    let timerCmd(timerInterval) =
        async { do! Async.Sleep (timerInterval * 1000)
                return TimedTick }
        |> Cmd.ofAsyncMsg

    let init () = initModel(), Cmd.none

    let update msg model =
        match msg with        
        | RestartBoard -> 
            let (newModel, cmd) = init()
            {newModel with timerOn = model.timerOn}, cmd

        | ResetConfig -> model, Cmd.none
        | ToggleCell cell -> model, Cmd.none
        | ToggleMode lifeMode -> 
            let newMode = 
                match model.mode with
                | RunMode -> ConfigureMode
                | ConfigureMode -> RunMode LifePaused
            {model with mode = newMode}, Cmd.none 
        | RunLife -> {model with timerOn = true}, timerCmd(model.timerInterval)
        | PauseLife -> {model with timerOn = false}, Cmd.none
        | StepLife -> 
            //update life board
            let lifeBoard = evolve(model.lifeBoard)                                        
            {model with lifeBoard = lifeBoard; stepCount = model.stepCount + 1}, Cmd.none
            
        | TimedTick -> 
            let lifeBoard, cmd = 
                match model.timerOn with 
                    | true -> 
                        //update life board - possibly this could be done while the timer was waiting
                        let newBoard = evolve(model.lifeBoard)
                        let cmd = 
                            match model.lifeBoard.changeFlag with
                            | true -> timerCmd(model.timerInterval)
                            | false -> Cmd.none
                        newBoard, cmd
                    | false -> model.lifeBoard, Cmd.none                
            {model with lifeBoard = lifeBoard; stepCount = model.stepCount + 1}, cmd
         | SetTimerInterval n -> {model with timerInterval = n;}, Cmd.none
         | ToggleTimer ->
            let (timerSetting, cmd) = 
                match model.timerOn with
                    | true -> (false, Cmd.none)
                    | false -> (true, timerCmd(model.timerInterval))
            {model with timerOn = timerSetting}, cmd


    let view (model: Model) dispatch =        
        let columns = model.lifeBoard.dimensions.columns
        let rows = model.lifeBoard.dimensions.rows
        let timerText = 
            match model.timerOn with
                | true -> "Stop timer"
                | false -> "Start timer"

        View.ContentPage(
            content = View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
                children =  
                    [ 
                    View.StackLayout(
                        orientation = StackOrientation.Horizontal, horizontalOptions = LayoutOptions.Fill,
                        children = [
                            View.Button(text="step", command=(fun() -> dispatch StepLife), horizontalOptions = LayoutOptions.Start)
                            View.Button(text="reset", command=(fun() -> dispatch RestartBoard), horizontalOptions = LayoutOptions.Center)
                            View.Button(text=timerText, command=(fun() -> dispatch ToggleTimer), horizontalOptions = LayoutOptions.End)                            
                        ]
                    )
                    View.Label(text=sprintf "Grid (%d*%d, auto version 0.0.2): %d" columns rows model.stepCount)
                    View.Grid(
                        backgroundColor=Color.AntiqueWhite, padding=Thickness 1.0, horizontalOptions=LayoutOptions.Fill,
                        coldefs=[for i in 1 .. columns -> Star], 
                        rowdefs= [for i in 1 .. rows -> Star],
                        children =  
                            //View.BoxView(color = Color.Aqua, horizontalOptions=LayoutOptions.Fill, verticalOptions=LayoutOptions.Fill).Row(1).Column(1)
                            List.mapi(fun index ((_, cell), _) ->
                                let colour = 
                                    match cell with
                                        | FullCell -> Color.Aqua
                                        | EmptyCell -> Color.White
                                let (i, j) = IndexToCartesian(index, model.lifeBoard.dimensions)
                                View.BoxView(color = colour, horizontalOptions=LayoutOptions.Fill, verticalOptions=LayoutOptions.Fill).Row(j).Column(i)
                            ) (model.lifeBoard.lifeArray |> List.ofArray)
                            (*for ((_, cell), _) in model.lifeBoard.lifeArray ->                            
                                let colour = 
                                    match cell with
                                        | FullCell -> Color.Aqua
                                        | EmptyCell -> Color.White
                                //let (i, j) = IndexToCartesian(ind)
                                View.BoxView(color = colour, horizontalOptions=LayoutOptions.Fill, verticalOptions=LayoutOptions.Fill).Row(i-1).Column(j-1)*)
                             )
                    ]
                )
          )

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


