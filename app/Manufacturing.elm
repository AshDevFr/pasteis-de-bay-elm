module Manufacturing
    exposing
        ( update
        , buyDough
        , adjustdoughCost
        , updateModel
        , makePasteis
        , tryMakePasteisModule
        , tryMakeMegaPasteisModule
        , enableMegaPasteisModule
        )

import Main.Msg as Main exposing (Msg(..))
import Main.Model as Main exposing (SaveModel, Model)
import Manufacturing.Msg as Manufacturing exposing (..)
import Business.Model exposing (BusinessModule)
import Manufacturing.Model exposing (ManufacturingModule, PasteisModule, MegaPasteisModule)
import Task exposing (..)


updateModel : ManufacturingModule -> BusinessModule -> ManufacturingModule
updateModel model business =
    { model
        | pasteisModule = (tryMakePasteisModule model business.funds)
        , megaPasteisModule = (tryMakeMegaPasteisModule model)
    }
        |> flip maybeBuyDough business.funds
        |> Tuple.first


tryMakePasteisModule : ManufacturingModule -> Float -> Maybe PasteisModule
tryMakePasteisModule model funds =
    case model.pasteisModule of
        Just mod ->
            let
                pasteisCost =
                    (1.1 ^ (toFloat mod.level)) + 4
            in
                Just { mod | cost = pasteisCost }

        Nothing ->
            let
                enoughFunds =
                    funds >= 5
            in
                case enoughFunds of
                    False ->
                        Nothing

                    True ->
                        Just
                            { cost = 5
                            , boost = 1
                            , level = 0
                            , factor = 1
                            }


tryMakeMegaPasteisModule : ManufacturingModule -> Maybe MegaPasteisModule
tryMakeMegaPasteisModule model =
    case model.megaPasteisModule of
        Just mod ->
            let
                megaPasteisCost =
                    (1.07 ^ (toFloat mod.level)) * 1000
            in
                Just { mod | cost = megaPasteisCost }

        Nothing ->
            Nothing


enableMegaPasteisModule : ManufacturingModule -> ManufacturingModule
enableMegaPasteisModule model =
    case model.megaPasteisModule of
        Just mod ->
            model

        Nothing ->
            { model
                | megaPasteisModule =
                    Just
                        { cost = 1000
                        , boost = 1
                        , level = 0
                        , factor = 500
                        }
            }


addPasteis : ManufacturingModule -> ManufacturingModule
addPasteis model =
    case model.pasteisModule of
        Nothing ->
            model

        Just mod ->
            { model | pasteisModule = addPasteis_ mod }


addPasteis_ : PasteisModule -> Maybe PasteisModule
addPasteis_ model =
    Just { model | level = model.level + 1 }


addMegaPasteis : ManufacturingModule -> ManufacturingModule
addMegaPasteis model =
    case model.megaPasteisModule of
        Nothing ->
            model

        Just mod ->
            { model | megaPasteisModule = addMegaPasteis_ mod }


addMegaPasteis_ : MegaPasteisModule -> Maybe MegaPasteisModule
addMegaPasteis_ model =
    Just { model | level = model.level + 1 }


adjustdoughCost : ManufacturingModule -> Float -> ManufacturingModule
adjustdoughCost model rand =
    if (rand > 1.5) then
        model
    else
        let
            doughAdjust =
                5 * (Basics.sin ((rand * 100) + 50))
        in
            { model
                | doughCost = ceiling (model.doughBasePrice + doughAdjust)
            }


noEffects : a -> ( a, Cmd msg )
noEffects =
    flip (,) Cmd.none


update : Manufacturing.Msg -> ManufacturingModule -> ( ManufacturingModule, Cmd Main.Msg )
update msg manufacturingModule =
    case msg of
        BakePastel availableDough ->
            createPastel manufacturingModule availableDough

        BuyDough funds ->
            buyDough manufacturingModule funds

        AddPasteis ->
            addPasteis manufacturingModule |> noEffects

        AddMegaPasteis ->
            addMegaPasteis manufacturingModule |> noEffects

        ToggleAutoBuy ->
            manufacturingModule.doughAutoBuy
                |> Maybe.map
                    (\doughAutoBuy ->
                        { manufacturingModule
                            | doughAutoBuy = Just (not doughAutoBuy)
                        }
                            |> noEffects
                    )
                |> Maybe.withDefault (manufacturingModule |> noEffects)


createPastel : ManufacturingModule -> Int -> ( ManufacturingModule, Cmd Main.Msg )
createPastel manufacturingModule availableDough =
    if availableDough < 1 then
        ( manufacturingModule, Cmd.none )
    else
        ( { manufacturingModule | dough = manufacturingModule.dough - 1 }
        , Task.perform NewPasteisBaked (Task.succeed 1)
        )


makePasteis : Model -> ( Model, Cmd Main.Msg )
makePasteis model =
    case model.manufacturingModule.dough of
        0 ->
            let
                manufacturingModule =
                    model.manufacturingModule

                newManufacturingModule =
                    { manufacturingModule
                        | pasteisMakerRate = 0
                        , partialPasteis = 0
                    }
            in
                ( { model
                    | manufacturingModule = newManufacturingModule
                  }
                , Cmd.none
                )

        _ ->
            let
                autoPasteisAmount =
                    moduleProduction model.manufacturingModule.pasteisModule

                megaPasteisAmount =
                    moduleProduction model.manufacturingModule.megaPasteisModule

                partialPasteisCapacity =
                    model.manufacturingModule.partialPasteis + autoPasteisAmount + megaPasteisAmount

                fullPasteisCapacity =
                    floor partialPasteisCapacity

                pasteisMakerRate =
                    (Basics.min (autoPasteisAmount + megaPasteisAmount) (toFloat model.manufacturingModule.dough)) * 10

                fullPasteis =
                    Basics.min fullPasteisCapacity model.manufacturingModule.dough

                manufacturingModule =
                    model.manufacturingModule

                newManufacturingModule =
                    { manufacturingModule
                        | partialPasteis = partialPasteisCapacity - (toFloat fullPasteisCapacity)
                        , dough = model.manufacturingModule.dough - fullPasteis
                        , pasteisMakerRate = pasteisMakerRate
                    }
            in
                ( { model
                    | manufacturingModule = newManufacturingModule
                  }
                , Task.perform NewPasteisBaked (Task.succeed fullPasteis)
                )


{-| Use an extensible record to enable row polymorphism
-}
type alias ProductionModule a =
    { a | boost : Float, level : Int, factor : Int }


moduleProduction : Maybe (ProductionModule m) -> Float
moduleProduction model =
    model
        |> Maybe.map
            (\mod ->
                0.1 * mod.boost * (toFloat <| mod.level * mod.factor)
            )
        |> Maybe.withDefault 0.0


buyDough : ManufacturingModule -> Float -> ( ManufacturingModule, Cmd Main.Msg )
buyDough manufacturingModule funds =
    let
        cost =
            toFloat manufacturingModule.doughCost
    in
        if (funds < cost) then
            ( manufacturingModule, Cmd.none )
        else
            ( { manufacturingModule
                | doughBasePrice = manufacturingModule.doughBasePrice + 0.05
                , dough = manufacturingModule.dough + (floor manufacturingModule.doughSupply)
              }
            , Task.perform DoughtBought (Task.succeed cost)
            )


maybeBuyDough : ManufacturingModule -> Float -> ( ManufacturingModule, Cmd Main.Msg )
maybeBuyDough manufacturingModule funds =
    case manufacturingModule.dough of
        0 ->
            manufacturingModule.doughAutoBuy
                |> Maybe.map
                    (\autoBuyer ->
                        case autoBuyer of
                            False ->
                                ( manufacturingModule, Cmd.none )

                            True ->
                                buyDough manufacturingModule funds
                    )
                |> Maybe.withDefault ( manufacturingModule, Cmd.none )

        _ ->
            ( manufacturingModule, Cmd.none )
