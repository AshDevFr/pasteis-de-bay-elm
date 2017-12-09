module Main.Update
    exposing
        ( updateModel
        , update
        , emptyModel
        , init
        , subscriptions
        )

import Main.Msg as Main exposing (..)
import Main.Model exposing (..)
import Random
import Time exposing (Time, every, second, millisecond)
import Json.Encode
import Json.Decode as Decode exposing (Decoder, decodeValue, map)
import Utils exposing (..)
import Validator exposing (decodeSaveModel, saveToModel, modelToSave)
import Cheats as Cheats
import Cheats.Model exposing (..)
import Business.Msg
import Business.Update as Business
import Manufacturing as Manufacturing exposing (..)
import Manufacturing.Init as Manufacturing
import Manufacturing.Msg
import Computing as Computing
import Task exposing (..)
import Dict
import Projects.Update as Projects
import Projects.Init as Projects exposing (tryMakeProjectsModule)
import Projects.Module.ProjectsModule exposing (ProjectsModule)
import Projects.Msg as Projects


type alias Simulations =
    { pasteisSimulation : Float
    , doughCostSimulation : Float
    }


emptyModel : Model
emptyModel =
    { lastTick = Nothing
    , pasteis = 0
    , businessModule = Business.init
    , manufacturingModule = Manufacturing.init
    , computingModule = Nothing
    , projectsModule = Nothing
    }


init : Maybe Decode.Value -> ( Model, Cmd Msg )
init savedModel =
    savedModel
        |> Maybe.map (decodeValue (decodeSaveModel |> Decode.map saveToModel))
        |> Maybe.map (Result.withDefault emptyModel)
        |> Maybe.withDefault emptyModel
        |> updateModel


updateModel : Model -> ( Model, Cmd Main.Msg )
updateModel model =
    let
        ( businessModule, cmds ) =
            Business.update Business.Msg.UpdateDemand model.businessModule

        manufacturingModule =
            Manufacturing.updateModel model.manufacturingModule businessModule

        computingModule =
            Computing.updateModel model
    in
        ( { model
            | businessModule = businessModule
            , manufacturingModule = manufacturingModule
            , computingModule = computingModule
          }
            |> Projects.tryMakeProjectsModule
            |> Computing.tryMakeComputingModule
            |> Projects.runTrigger
        , cmds
        )


update : (SaveModel -> Cmd Msg) -> Msg -> Model -> ( Model, Cmd Msg )
update savePort msg model =
    case msg of
        ManufacturingMessage manufacturingMessage ->
            let
                ( newManufacturingModule, cmd ) =
                    Manufacturing.update manufacturingMessage model.manufacturingModule
            in
                ( { model | manufacturingModule = newManufacturingModule }, cmd )

        BusinessMessage businessMessage ->
            let
                ( newBusinessModule, cmd ) =
                    Business.update businessMessage model.businessModule
            in
                ( { model | businessModule = newBusinessModule }, cmd )

        ProjectsMessage projectsMessage ->
            model.projectsModule
                |> Maybe.map
                    (\mod ->
                        let
                            ( newProjectsModule, cmd ) =
                                Projects.update projectsMessage mod
                        in
                            ( { model | projectsModule = Just newProjectsModule }, cmd )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        Tick newTime ->
            let
                ( mod, cmds ) =
                    applyTime model newTime
            in
                ( mod
                , Cmd.batch
                    [ savePort (modelToSave model)
                    , cmds
                    ]
                )

        BuyPasteis ->
            model.manufacturingModule.pasteisModule
                |> Maybe.map
                    (\mod ->
                        ( model
                        , Cmd.batch
                            [ Task.perform BusinessMessage <| Task.succeed (Business.Msg.RemoveFunds mod.cost)
                            , Task.perform ManufacturingMessage <| Task.succeed (Manufacturing.Msg.AddPasteis)
                            ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        BuyMegaPasteis ->
            model.manufacturingModule.megaPasteisModule
                |> Maybe.map
                    (\mod ->
                        ( model
                        , Cmd.batch
                            [ Task.perform BusinessMessage <| Task.succeed (Business.Msg.RemoveFunds mod.cost)
                            , Task.perform ManufacturingMessage <| Task.succeed (Manufacturing.Msg.AddMegaPasteis)
                            ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        AddProcessor ->
            model.computingModule
                |> Maybe.map
                    (\computingModule ->
                        { model
                            | computingModule = Just (Computing.addProcessor computingModule)
                        }
                    )
                |> Maybe.withDefault model
                |> flip (,) Cmd.none

        AddMemory ->
            model.computingModule
                |> Maybe.map
                    (\computingModule ->
                        { model
                            | computingModule = Just (Computing.addMemory computingModule)
                        }
                    )
                |> Maybe.withDefault model
                |> flip (,) Cmd.none

        Reset ->
            ( emptyModel, Cmd.none )

        UpdateModel ->
            updateModel model

        --, Cmd.none )
        NewPasteisBaked cnt ->
            let
                ( bm, cmds ) =
                    Business.update (Business.Msg.AddItems cnt) model.businessModule

                newModel =
                    { model
                        | businessModule = bm
                        , pasteis = model.pasteis + cnt
                    }
            in
                ( newModel, cmds )

        DoughtBought cost ->
            ( model
            , Task.perform BusinessMessage <| Task.succeed (Business.Msg.RemoveFunds cost)
            )

        ActivateProject project ->
            let
                updateProjectsModule : String -> ProjectsModule -> ProjectsModule
                updateProjectsModule projectId mod =
                    { mod | projectsActivated = Dict.insert projectId True mod.projectsActivated }

                updateModelWithProjectsModule : ProjectsModule -> Model
                updateModelWithProjectsModule pm =
                    { model | projectsModule = Just pm }

                applyEffects : List Projects.Msg -> Model -> Model
                applyEffects lst model =
                    let
                        doApply : Projects.Msg -> Model -> Model
                        doApply msg model =
                            case msg of
                                Projects.MapPasteisBoost fn ->
                                    let
                                        manufacturingModule =
                                            model.manufacturingModule

                                        newPasteisModule =
                                            manufacturingModule.pasteisModule
                                                |> Maybe.map
                                                    (\mod -> { mod | boost = fn mod.boost })

                                        newManufacturingModule =
                                            { manufacturingModule
                                                | pasteisModule = newPasteisModule
                                            }
                                    in
                                        { model | manufacturingModule = newManufacturingModule }

                                Projects.MapMegaPasteisBoost fn ->
                                    let
                                        manufacturingModule =
                                            model.manufacturingModule

                                        newMegaPasteisModule =
                                            manufacturingModule.megaPasteisModule
                                                |> Maybe.map
                                                    (\mod -> { mod | boost = fn mod.boost })

                                        newManufacturingModule =
                                            { manufacturingModule
                                                | megaPasteisModule = newMegaPasteisModule
                                            }
                                    in
                                        { model | manufacturingModule = newManufacturingModule }

                                Projects.MapDoughSupply fn ->
                                    let
                                        manufacturingModule =
                                            model.manufacturingModule

                                        newManufacturingModule =
                                            { manufacturingModule
                                                | doughSupply = fn manufacturingModule.doughSupply
                                            }
                                    in
                                        { model | manufacturingModule = newManufacturingModule }

                                Projects.MapMarketingEffectiveness fn ->
                                    let
                                        businessModule =
                                            model.businessModule

                                        newBusinessModule =
                                            { businessModule
                                                | marketingEffectiveness = fn businessModule.marketingEffectiveness
                                            }
                                    in
                                        { model | businessModule = newBusinessModule }

                                Projects.MapDemandBoost fn ->
                                    let
                                        businessModule =
                                            model.businessModule

                                        newBusinessModule =
                                            { businessModule
                                                | demandBoost = fn businessModule.demandBoost
                                            }
                                    in
                                        { model | businessModule = newBusinessModule }

                                Projects.MapFunds fn ->
                                    let
                                        businessModule =
                                            model.businessModule

                                        newBusinessModule =
                                            { businessModule
                                                | funds = fn businessModule.funds
                                            }
                                    in
                                        { model | businessModule = newBusinessModule }

                                Projects.MapOperations fn ->
                                    model.computingModule
                                        |> Maybe.map
                                            ((\mod -> { mod | operations = fn mod.operations })
                                                >> (\cm -> { model | computingModule = Just cm })
                                            )
                                        |> Maybe.withDefault model

                                Projects.MapEnableCreativity ->
                                    model.computingModule
                                        |> Maybe.map
                                            ((\mod ->
                                                { mod | creativity = Just (mod.creativity |> Maybe.withDefault 0) }
                                             )
                                                >> (\cm -> { model | computingModule = Just cm })
                                            )
                                        |> Maybe.withDefault model

                                Projects.MapEnableStats ->
                                    { model | businessModule = (Business.enableStats model.businessModule) }

                                Projects.MapEnableMegaPasteis ->
                                    { model
                                        | manufacturingModule = Manufacturing.enableMegaPasteisModule model.manufacturingModule
                                    }

                                Projects.MapDoughAutoBuy ->
                                    let
                                        manufacturingModule =
                                            model.manufacturingModule

                                        newManufacturingModule =
                                            { manufacturingModule
                                                | doughAutoBuy = Just True
                                            }
                                    in
                                        { model | manufacturingModule = newManufacturingModule }

                                Projects.MapCreativity fn ->
                                    model.computingModule
                                        |> Maybe.map
                                            ((\mod ->
                                                (mod.creativity
                                                    |> Maybe.map
                                                        (\creativity ->
                                                            { mod | creativity = Just (fn creativity) }
                                                        )
                                                    |> Maybe.withDefault mod
                                                )
                                             )
                                                >> (\cm -> { model | computingModule = Just cm })
                                            )
                                        |> Maybe.withDefault model

                                Projects.MapTrust fn ->
                                    model.computingModule
                                        |> Maybe.map
                                            ((\mod -> { mod | trust = fn mod.trust })
                                                >> (\cm -> { model | computingModule = Just cm })
                                            )
                                        |> Maybe.withDefault model

                                _ ->
                                    model
                    in
                        List.foldl doApply model lst
            in
                Projects.buy model project.cost
                    |> Result.map
                        (\model ->
                            model.projectsModule
                                |> Maybe.map
                                    (updateProjectsModule project.id
                                        >> updateModelWithProjectsModule
                                        >> (applyEffects project.effect)
                                    )
                                |> Maybe.withDefault model
                        )
                    |> Result.withDefault model
                    |> flip (,) Cmd.none

        ApplyCheat cheat ->
            ( Cheats.execute model cheat, Cmd.none )


applyTime : Model -> Time -> ( Model, Cmd Msg )
applyTime model time =
    let
        setLastTick : Model -> Time -> Model
        setLastTick model time =
            { model | lastTick = Just time }

        seed0 =
            Random.initialSeed (floor (Time.inMilliseconds time))

        operationsToRun =
            model.lastTick
                |> Maybe.andThen
                    (\lastTick ->
                        let
                            elapsedTime =
                                (Time.inMilliseconds time) - (Time.inMilliseconds lastTick)
                        in
                            Just <| Basics.min (Basics.max (floor (elapsedTime / 100)) 1) 36000
                    )
                |> Maybe.withDefault 1

        step : Int -> ( ( Model, Cmd Msg ), Random.Seed ) -> ( ( Model, Cmd Msg ), Random.Seed )
        step it ( ( model, previousStepCmds ), seed ) =
            let
                ( pasteisSimulation, seed1 ) =
                    Utils.randomFloat 0 100 seed

                ( doughCostSimulation, seed2 ) =
                    Utils.randomFloat 0 100 seed1
            in
                ( applyTime_ model previousStepCmds (Simulations pasteisSimulation doughCostSimulation), seed2 )

        range =
            List.range 1 operationsToRun
    in
        setLastTick model time
            |> \updatedModel ->
                List.foldl step ( ( updatedModel, Cmd.none ), seed0 ) range
                    |> Tuple.first


applyTime_ : Model -> Cmd Msg -> Simulations -> ( Model, Cmd Msg )
applyTime_ model previousStepCmds { pasteisSimulation, doughCostSimulation } =
    let
        ( bm, businessCmds ) =
            Business.update (Business.Msg.SellPasteis pasteisSimulation) model.businessModule
    in
        { model
            | businessModule = bm
            , manufacturingModule = Manufacturing.adjustdoughCost model.manufacturingModule doughCostSimulation
        }
            |> Manufacturing.makePasteis
            |> (\( mod, mkPasteisCms ) ->
                    makeOperations mod
                        |> (\( mod2, mkOpsCmds ) -> ( mod2, Cmd.batch [ previousStepCmds, businessCmds, mkPasteisCms, mkOpsCmds ] ))
               )
            |> (\( mod, mkOpsCmds ) ->
                    updateModel mod
                        |> (\( mod2, updateCmds ) -> ( mod2, Cmd.batch [ mkOpsCmds, updateCmds ] ))
               )


makeOperations : Model -> ( Model, Cmd msg )
makeOperations model =
    ( { model
        | computingModule =
            model.computingModule
                |> Maybe.map Computing.makeOperations
      }
    , Cmd.none
    )


subscriptions : ((Json.Encode.Value -> Msg) -> Sub Msg) -> Model -> Sub Msg
subscriptions cheats model =
    Sub.batch
        [ every (100 * millisecond) Tick
        , cheats (toCheat >> ApplyCheat)
        ]


addFundsDecoder : Decoder CheatModel
addFundsDecoder =
    Decode.map AddFunds (Decode.field "funds" Decode.int)


addOpsDecoder : Decoder CheatModel
addOpsDecoder =
    Decode.map AddOps (Decode.field "operations" Decode.int)


addItemsDecoder : Decoder CheatModel
addItemsDecoder =
    Decode.map AddItems (Decode.field "quantity" Decode.int)


addAutoPasteisDecoder : Decoder CheatModel
addAutoPasteisDecoder =
    Decode.map AddAutoPasteis (Decode.field "quantity" Decode.int)


addAutoMegaPasteisDecoder : Decoder CheatModel
addAutoMegaPasteisDecoder =
    Decode.map AddAutoMegaPasteis (Decode.field "quantity" Decode.int)


addDoughDecoder : Decoder CheatModel
addDoughDecoder =
    Decode.map AddDough (Decode.field "quantity" Decode.int)


addCreativityDecoder : Decoder CheatModel
addCreativityDecoder =
    Decode.map AddCreativity (Decode.field "quantity" Decode.int)


cheatFromType : String -> Decoder CheatModel
cheatFromType string =
    case string of
        "addFunds" ->
            addFundsDecoder

        "addOps" ->
            addOpsDecoder

        "addItems" ->
            addItemsDecoder

        "addAutoPasteis" ->
            addAutoPasteisDecoder

        "addAutoMegaPasteis" ->
            addAutoMegaPasteisDecoder

        "addDough" ->
            addDoughDecoder

        "addCreativity" ->
            addCreativityDecoder

        _ ->
            Decode.fail ("Invalid cheat type: " ++ string)


cheatDecoder : Decoder CheatModel
cheatDecoder =
    Decode.field "action" Decode.string
        |> Decode.andThen cheatFromType


toCheat : Json.Encode.Value -> CheatModel
toCheat encodedValue =
    encodedValue
        |> Decode.decodeValue cheatDecoder
        |> Result.withDefault DoNothing
