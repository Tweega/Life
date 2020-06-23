// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Life

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

module App =
    type PlayMode =
        | RunMode
        | ConfigureMode 

    type Model = 
      {
        rows: int
        columns: int
        mode: PlayMode
      }
    type Cell = int * int
    
    type Msg = 
        //configure mode
        | ToggleCell of Cell    
        | Reset
        | RunLife

        // play mode
        | PauseEvolution
        | CeaseEvolution
        | StepEvolution 
        | SetStep
        | TimedTick
        
        // both modes
        | ChangeMode of PlayMode
    let initModel = { rows = 10; columns = 10; mode = ConfigureMode}

    let init () = initModel, Cmd.none

    let timerCmd =
        async { do! Async.Sleep 200
                return TimedTick }
        |> Cmd.ofAsyncMsg

    let update msg model =
        match msg with
        
        
        | Reset -> init ()        
        | ToggleCell cell -> model, Cmd.none
        | ChangeMode lifeMode -> model, Cmd.none        
        | PauseEvolution -> model, Cmd.none
        | StepEvolution -> model, Cmd.none
        | CeaseEvolution -> model, Cmd.none
        | SetStep -> model, Cmd.none
        | TimedTick -> model, Cmd.none
        | RunLife -> model, Cmd.none
                

    let view (model: Model) dispatch =        
        View.ContentPage(
            content = View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
                children =  
                    [ 
                    View.Label(text=sprintf "Grid (6x6, auto version 0.0.4):")
                    View.Grid(
                        backgroundColor=Color.Black, padding=Thickness 3.0, horizontalOptions=LayoutOptions.Fill,
                        coldefs=[for i in 1 .. model.columns -> Star], 
                        rowdefs= [for i in 1 .. model.rows -> Star],
                        children = [ 
                            for i in 1 .. model.rows do 
                                for j in 1 .. model.columns -> 
                                    let c = Color((1.0/float i), (1.0/float j), (1.0/float (i+j)), 1.0)
                                    View.BoxView(color = c, horizontalOptions=LayoutOptions.Fill, verticalOptions=LayoutOptions.Fill).Row(i-1).Column(j-1)
                            ] )
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


