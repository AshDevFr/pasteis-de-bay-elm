port module Main exposing (..)

import Html exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, Device(..), Align(..), align)
import Time exposing (Time, every, second, millisecond)
import Random
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Models exposing (..)
import Utils exposing (..)
import Business as Business
import Manufacturing as Manufacturing
import Computing as Computing


main : Program (Maybe SaveModel) Model Msg
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


init : Maybe SaveModel -> ( Model, Cmd Msg )
init savedModel =
    case savedModel of
        Nothing ->
            ( emptyModel, Cmd.none )

        Just mod ->
            ( updateModel (Utils.saveToModel mod), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreatePastel ->
            ( Manufacturing.createPastel model, Cmd.none )

        BuyDough ->
            let
                ( manufacturingModule, businessModule ) =
                    Manufacturing.buyDough model.manufacturingModule model.businessModule
            in
                ( { model
                    | businessModule = businessModule
                    , manufacturingModule = manufacturingModule
                  }
                , Cmd.none
                )

        LowerPrice ->
            ( { model
                | businessModule = Business.lowerPrice model.businessModule
              }
                |> updateModel
            , Cmd.none
            )

        RaisePrice ->
            ( { model
                | businessModule = Business.raisePrice model.businessModule
              }
                |> updateModel
            , Cmd.none
            )

        BuyAds ->
            ( { model
                | businessModule = Business.buyAds model.businessModule
              }
            , Cmd.none
            )

        Tick newTime ->
            ( applyTime model newTime
            , Cmd.batch
                [ saveState (Utils.modelToSave model)
                ]
            )

        BuyPasteis ->
            case model.manufacturingModule.pasteisModule of
                Nothing ->
                    ( model, Cmd.none )

                Just mod ->
                    { model
                        | businessModule = Business.removeFunds model.businessModule mod.cost
                        , manufacturingModule = Manufacturing.addPasteis model.manufacturingModule
                    }
                        |> flip (,) Cmd.none

        BuyMegaPasteis ->
            case model.manufacturingModule.megaPasteisModule of
                Nothing ->
                    ( model, Cmd.none )

                Just mod ->
                    { model
                        | businessModule = Business.removeFunds model.businessModule mod.cost
                        , manufacturingModule = Manufacturing.addMegaPasteis model.manufacturingModule
                    }
                        |> flip (,) Cmd.none

        AddProcessor ->
            case model.computingModule of
                Nothing ->
                    ( model, Cmd.none )

                Just computingModule ->
                    { model
                        | computingModule = Just (Computing.addProcessor computingModule)
                    }
                        |> flip (,) Cmd.none

        AddMemory ->
            case model.computingModule of
                Nothing ->
                    ( model, Cmd.none )

                Just computingModule ->
                    { model
                        | computingModule = Just (Computing.addMemory computingModule)
                    }
                        |> flip (,) Cmd.none

        Reset ->
            ( emptyModel, Cmd.none )

        UpdateModel ->
            ( updateModel model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every (100 * millisecond) Tick
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text ("Pasteis " ++ (formatInt usLocale model.pasteis))
            ]
        , grid []
            [ cell [ size All 12 ]
                [ Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Button.raised
                    , Button.ripple
                    , Options.onClick CreatePastel
                    , Options.disabled (model.manufacturingModule.dough < 1)
                    ]
                    [ text "Make a Pastel" ]
                , text ""
                , Button.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Button.minifab
                    , Button.colored
                    , Button.ripple
                    , Options.onClick Reset
                    ]
                    [ Icon.i "delete" ]
                ]
            , cell [ size All 3 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-end"
                    , css "margin-top" "20px"
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "space-between"
                        , css "align-items" "center"
                        , css "min-width" "256px"
                        , css "flex" "1 1 auto"
                        ]
                        [ Business.view model
                        , Manufacturing.view model
                        ]
                    ]
                ]
            , cell [ size All 3 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-end"
                    , css "margin-top" "20px"
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "space-between"
                        , css "align-items" "center"
                        , css "min-width" "256px"
                        , css "flex" "1 1 auto"
                        ]
                        [ Computing.view model
                        ]
                    ]
                ]
            , cell [ size All 3 ]
                [ Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-end"
                    , css "margin-top" "20px"
                    ]
                    [ Options.div
                        [ css "display" "flex"
                        , css "flex-flow" "row wrap"
                        , css "justify-content" "space-between"
                        , css "align-items" "center"
                        , css "min-width" "256px"
                        , css "flex" "1 1 auto"
                        ]
                        []
                    ]
                ]
            ]
        ]
        |> Material.Scheme.top


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
            case model.lastTick of
                Nothing ->
                    1

                Just lastTick ->
                    let
                        elapsedTime =
                            (Time.inMilliseconds time) - (Time.inMilliseconds lastTick)
                    in
                        Basics.max (floor (elapsedTime / 100)) 1

        ( pasteisSimulations, seed1 ) =
            Utils.randomMultipleFloat 0 100 operationsToRun seed0

        ( doughCostSimulations, seed2 ) =
            Utils.randomMultipleFloat 0 100 operationsToRun seed1

        updatedModel =
            setLastTick model time

        allSimulations =
            List.map2 Simulations pasteisSimulations doughCostSimulations

        reducer simulations model =
            applyTime_ model simulations
    in
        List.foldl reducer updatedModel allSimulations


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
    case model.computingModule of
        Nothing ->
            model

        Just mod ->
            { model | computingModule = Just (Computing.makeOperations mod) }
