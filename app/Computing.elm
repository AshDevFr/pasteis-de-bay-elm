module Computing
    exposing
        ( init
        , view
        , addProcessor
        , addMemory
        , updateModel
        , tryMakeComputingModule
        , makeOperations
        , mapOperations
        )

import Html exposing (Html, div, span, button, text, h2, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import Models exposing (..)
import FormatNumber exposing (formatFloat, formatInt, usLocale)


init : ComputingModule
init =
    { trust = 2
    , processors = 1
    , memory = 1
    , memoryLimit = 1000
    , operations = 0
    , creativity = Nothing
    }


view : Model -> Html Msg
view model =
    case model.computingModule of
        Nothing ->
            text ""

        Just mod ->
            div
                [ style
                    [ ( "margin", "4px 8px" )
                    , ( "width", "100%" )
                    ]
                ]
                [ div
                    [ style [ ( "flex-direction", "column" ) ] ]
                    [ h2 [] [ text "Research" ]
                    , h3 [] [ text ("Trust : " ++ (formatInt usLocale mod.trust)) ]
                    ]
                , div
                    []
                    [ div []
                        [ text ("Next trust " ++ (formatInt usLocale ((nextTrust mod.trust + 1) * 1000)))
                        ]
                    ]
                , div
                    [ style [ ( "margin-top", "10px" ) ] ]
                    [ div []
                        [ button
                            [ onClick AddProcessor
                            , disabled ((mod.trust - (mod.processors + mod.memory)) < 1)
                            ]
                            [ text "Processors"
                            ]
                        , text (" " ++ (formatInt usLocale mod.processors))
                        ]
                    , div []
                        [ button
                            [ onClick AddMemory
                            , disabled ((mod.trust - (mod.processors + mod.memory)) < 1)
                            ]
                            [ text "Memory"
                            ]
                        , text (" " ++ (formatInt usLocale mod.memory))
                        ]
                    ]
                , div
                    [ style [ ( "margin-top", "10px" ) ] ]
                    [ div []
                        [ text
                            ("Operations : "
                                ++ (formatInt usLocale (floor mod.operations))
                                ++ " / "
                                ++ (formatInt usLocale mod.memoryLimit)
                            )
                        ]
                    , div
                        []
                        [ text ("Creativity : " ++ (formatInt usLocale (floor (mod.creativity |> Maybe.withDefault 0.0))))
                        ]
                    ]
                ]


updateModel : Model -> Maybe ComputingModule
updateModel model =
    case model.computingModule of
        Nothing ->
            Nothing

        Just mod ->
            let
                addTrust =
                    model.pasteis >= ((nextTrust mod.trust + 1) * 1000)
            in
                case addTrust of
                    False ->
                        Just
                            { mod
                                | memoryLimit = mod.memory * 1000
                            }

                    True ->
                        Just
                            { mod
                                | memoryLimit = mod.memory * 1000
                                , trust = mod.trust + 1
                            }


tryMakeComputingModule : Model -> Model
tryMakeComputingModule model =
    case model.computingModule of
        Just mod ->
            model

        Nothing ->
            let
                enoughPasteis =
                    model.pasteis >= 2000
            in
                case enoughPasteis of
                    False ->
                        model

                    True ->
                        { model | computingModule = Just init }


addProcessor : ComputingModule -> ComputingModule
addProcessor model =
    let
        availableTrust =
            (model.trust - (model.processors + model.memory)) > 0
    in
        case availableTrust of
            False ->
                model

            True ->
                { model
                    | processors = model.processors + 1
                }


addMemory : ComputingModule -> ComputingModule
addMemory model =
    let
        availableTrust =
            (model.trust - (model.processors + model.memory)) > 0
    in
        case availableTrust of
            False ->
                model

            True ->
                { model
                    | memory = model.memory + 1
                }


makeOperations : ComputingModule -> ComputingModule
makeOperations model =
    let
        full =
            model.operations >= (toFloat model.memoryLimit)
    in
        case full of
            True ->
                model |> makeCreativity

            False ->
                let
                    newOps =
                        (toFloat model.processors) / 10

                    operations =
                        Basics.min (model.operations + newOps) (toFloat model.memoryLimit)
                in
                    { model | operations = operations }


mapOperations : ComputingModule -> (Float -> Float) -> ComputingModule
mapOperations model fn =
    let
        newVal =
            fn model.operations

        tooHigh =
            newVal >= (toFloat model.memoryLimit)

        tooLow =
            newVal < 0.0
    in
        case tooHigh || tooLow of
            True ->
                model

            False ->
                { model | operations = newVal }


creativitySpeed : Float -> Float
creativitySpeed processorsCount =
    (logBase 10 processorsCount)
        * (processorsCount ^ 1.1)
        + (processorsCount - 1)
        |> flip (/) 400


makeCreativity : ComputingModule -> ComputingModule
makeCreativity model =
    model.creativity
        |> Maybe.map ((+) (creativitySpeed (model.processors |> toFloat)))
        |> Maybe.map (\newCreativity -> { model | creativity = Just newCreativity })
        |> Maybe.withDefault model


nextTrust : Int -> Int
nextTrust x =
    case x of
        0 ->
            1

        1 ->
            1

        _ ->
            nextTrust (x - 1) + nextTrust (x - 2)
