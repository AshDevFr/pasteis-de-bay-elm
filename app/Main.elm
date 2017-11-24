port module Main exposing (..)

import Html exposing (..)
import Material
import Time exposing (Time, every, second, millisecond)
import Random
import Json.Decode as Decode exposing (decodeValue, map)
import Models exposing (..)
import Utils exposing (..)
import Business as Business
import Business.Msg
import Manufacturing as Manufacturing exposing (..)
import Manufacturing.Msg
import Computing as Computing
import Views.Main as MainView
import Task exposing (..)


main : Program (Maybe Decode.Value) Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port saveState : SaveModel -> Cmd msg


emptyModel : Model
emptyModel =
    { mdl = Material.model
    , lastTick = Nothing
    , pasteis = 0
    , businessModule = Business.init
    , manufacturingModule = Manufacturing.init
    , computingModule = Nothing
    }


init : Maybe Decode.Value -> ( Model, Cmd Msg )
init savedModel =
    savedModel
        |> Maybe.map (decodeValue (decodeSaveModel |> Decode.map Utils.saveToModel))
        |> Maybe.map (Result.withDefault emptyModel)
        |> Maybe.withDefault emptyModel
        |> updateModel
        |> flip (,) Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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

        Tick newTime ->
            ( applyTime model newTime
            , Cmd.batch
                [ saveState (Utils.modelToSave model)
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
            ( updateModel model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        NewPasteisBaked cnt ->
            let
                newModel =
                    { model
                        | businessModule = Business.addItems model.businessModule cnt
                        , pasteis = model.pasteis + cnt
                    }
            in
                ( newModel, Cmd.none )

        DoughtBought cost ->
            ( model
            , Task.perform BusinessMessage <| Task.succeed (Business.Msg.RemoveFunds cost)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every (100 * millisecond) Tick
        ]


updateModel : Model -> Model
updateModel model =
    let
        businessModule =
            Business.updateModel model.businessModule

        manufacturingModule =
            Manufacturing.updateModel model.manufacturingModule businessModule

        computingModule =
            Computing.updateModel model
    in
        { model
            | businessModule = businessModule
            , manufacturingModule = manufacturingModule
            , computingModule = computingModule
        }
            |> Computing.tryMakeComputingModule


type alias Simulations =
    { pasteisSimulation : Float
    , doughCostSimulation : Float
    }


applyTime : Model -> Time -> Model
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
                            Just <| Basics.max (floor (elapsedTime / 100)) 1
                    )
                |> Maybe.withDefault 1

        step : Int -> ( Model, Random.Seed ) -> ( Model, Random.Seed )
        step it ( model, seed ) =
            let
                ( pasteisSimulation, seed1 ) =
                    Utils.randomFloat 0 100 seed

                ( doughCostSimulation, seed2 ) =
                    Utils.randomFloat 0 100 seed1
            in
                ( applyTime_ model (Simulations pasteisSimulation doughCostSimulation), seed2 )

        range =
            List.range 1 operationsToRun
    in
        setLastTick model time
            |> \updatedModel ->
                List.foldl step ( updatedModel, seed0 ) range
                    |> Tuple.first


view : Model -> Html Msg
view =
    MainView.view


applyTime_ : Model -> Simulations -> Model
applyTime_ model { pasteisSimulation, doughCostSimulation } =
    { model
        | businessModule = Business.sellPasteis model.businessModule pasteisSimulation
        , manufacturingModule = Manufacturing.adjustdoughCost model.manufacturingModule doughCostSimulation
    }
        |> Manufacturing.makePasteis
        |> makeOperations
        |> updateModel


makeOperations : Model -> Model
makeOperations model =
    { model
        | computingModule =
            model.computingModule
                |> Maybe.map Computing.makeOperations
    }
