module Computing
    exposing
        ( addProcessor
        , addMemory
        , updateModel
        , tryMakeComputingModule
        , makeOperations
        , mapOperations
        , nextTrust
        )

import Computing.Init exposing (init)
import Computing.Model exposing (ComputingModule)
import Main.Model exposing (Model)


updateModel : Model -> Maybe ComputingModule
updateModel model =
    model.computingModule
        |> Maybe.map
            (\mod ->
                let
                    addTrust =
                        model.pasteis >= ((nextTrust mod.trust + 1) * 1000)
                in
                    case addTrust of
                        False ->
                            { mod
                                | memoryLimit = mod.memory * 1000
                            }

                        True ->
                            { mod
                                | memoryLimit = mod.memory * 1000
                                , trust = mod.trust + 1
                            }
            )


tryMakeComputingModule : Model -> Model
tryMakeComputingModule model =
    model.computingModule
        |> Maybe.map (\mod -> model)
        |> Maybe.withDefault
            (let
                enoughPasteis =
                    model.pasteis >= 2000
             in
                case enoughPasteis of
                    False ->
                        model

                    True ->
                        { model | computingModule = Just init }
            )


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
                        (toFloat model.processors)

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
        |> flip (/) 40


makeCreativity : ComputingModule -> ComputingModule
makeCreativity model =
    model.creativity
        |> Maybe.map
            (((+) (creativitySpeed (model.processors |> toFloat)))
                >> (\newCreativity -> { model | creativity = Just newCreativity })
            )
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
