module Cheats
    exposing
        ( execute
        )

import Main.Model exposing (Model)
import Cheats.Model exposing (CheatModel)
import Manufacturing as Manufacturing


execute : Model -> CheatModel -> Model
execute model cheat =
    case cheat.action of
        "addFunds" ->
            addFunds model cheat.amount

        "addItems" ->
            addItems model cheat.amount

        "addAutoPasteis" ->
            addAutoPasteis model cheat.amount

        "addAutoMegaPasteis" ->
            addAutoMegaPasteis model cheat.amount

        "addDough" ->
            addDough model cheat.amount

        "addOps" ->
            addOps model cheat.amount

        "addCreativity" ->
            addCreativity model cheat.amount

        _ ->
            model


addItems : Model -> Int -> Model
addItems model amount =
    let
        businessModule =
            model.businessModule

        newBusinessModule =
            { businessModule
                | inventory = businessModule.inventory + amount
            }
    in
        { model
            | pasteis = model.pasteis + amount
            , businessModule = newBusinessModule
        }


addFunds : Model -> Int -> Model
addFunds model amount =
    let
        businessModule =
            model.businessModule

        newBusinessModule =
            { businessModule
                | funds = businessModule.funds + (toFloat amount)
            }
    in
        { model
            | businessModule = newBusinessModule
        }


addAutoPasteis : Model -> Int -> Model
addAutoPasteis model amount =
    let
        manufacturingModule =
            model.manufacturingModule

        pasteisModule =
            Manufacturing.tryMakePasteisModule manufacturingModule 1000

        newPasteisModule =
            pasteisModule |> Maybe.map (\mod -> { mod | level = mod.level + amount })

        newManufacturingModule =
            { manufacturingModule
                | pasteisModule = newPasteisModule
            }
    in
        { model
            | manufacturingModule = newManufacturingModule
        }


addAutoMegaPasteis : Model -> Int -> Model
addAutoMegaPasteis model amount =
    let
        manufacturingModule =
            model.manufacturingModule

        megaPasteisModule =
            Manufacturing.tryMakeMegaPasteisModule manufacturingModule

        newMegaPasteisModule =
            megaPasteisModule |> Maybe.map (\mod -> { mod | level = mod.level + amount })

        newManufacturingModule =
            { manufacturingModule
                | megaPasteisModule = newMegaPasteisModule
            }
    in
        { model
            | manufacturingModule = newManufacturingModule
        }


addDough : Model -> Int -> Model
addDough model amount =
    let
        manufacturingModule =
            model.manufacturingModule

        newManufacturingModule =
            { manufacturingModule
                | dough = manufacturingModule.dough + amount
            }
    in
        { model
            | manufacturingModule = newManufacturingModule
        }


addOps : Model -> Int -> Model
addOps model amount =
    model.computingModule
        |> Maybe.map
            (\mod ->
                let
                    newComputingModule =
                        Just
                            { mod
                                | operations = mod.operations + (toFloat amount)
                            }
                in
                    { model | computingModule = newComputingModule }
            )
        |> Maybe.withDefault model


addCreativity : Model -> Int -> Model
addCreativity model amount =
    model.computingModule
        |> Maybe.map
            (\mod ->
                let
                    creativity =
                        Maybe.withDefault 0 mod.creativity

                    newComputingModule =
                        Just
                            { mod
                                | creativity = Just (creativity + (toFloat amount))
                            }
                in
                    { model | computingModule = newComputingModule }
            )
        |> Maybe.withDefault model
