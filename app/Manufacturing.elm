module Manufacturing
    exposing
        ( init
        , update
        , view
        , buyDough
        , adjustdoughCost
        , updateModel
        , makePasteis
        , tryMakePasteisModule
        , tryMakeMegaPasteisModule
        )

import Html exposing (Html, div, span, button, text, h2, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Business as Business
import Main.Msg exposing (Msg(..))
import Manufacturing.Msg as Manufacturing exposing (..)
import Business.Model exposing (BusinessModule)
import Manufacturing.Model exposing (ManufacturingModule, PasteisModule, MegaPasteisModule)
import Main.Model exposing (SaveModel, Model)
import Task exposing (..)


init : ManufacturingModule
init =
    { dough = 1000
    , doughSupply = 1000
    , doughCost = 15
    , doughBasePrice = 15
    , pasteisModule = Nothing
    , megaPasteisModule = Nothing
    , partialPasteis = 0
    , pasteisMakerRate = 0
    }


view : Model -> Html Main.Msg.Msg
view model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        div
            [ style
                [ ( "margin", "4px 8px" )
                , ( "width", "100%" )
                ]
            ]
            [ div
                [ style [ ( "flex-direction", "column" ) ] ]
                [ h2 [] [ text "Manufacturing" ]
                , h3 [] [ text ("Pasteis per Second: " ++ (manufacturingModule.pasteisMakerRate |> round |> toString)) ]
                ]
            , div
                []
                [ div []
                    [ div []
                        [ button
                            [ onClick (ManufacturingMessage (Manufacturing.BuyDough businessModule.funds))
                            , disabled (businessModule.funds < (toFloat manufacturingModule.doughCost))
                            ]
                            [ text "Dough"
                            ]
                        , text (" " ++ (formatInt usLocale manufacturingModule.dough) ++ " coins")
                        ]
                    , div
                        []
                        [ text ("Cost: $ " ++ (toString manufacturingModule.doughCost))
                        ]
                    ]
                ]
            , pasteisView model
            , megaPasteisView model
            ]


pasteisView : Model -> Html Main.Msg.Msg
pasteisView model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        manufacturingModule.pasteisModule
            |> Maybe.map
                (\mod ->
                    div
                        [ style [ ( "margin-top", "10px" ) ] ]
                        [ div []
                            [ div []
                                [ button
                                    [ onClick BuyPasteis
                                    , disabled (businessModule.funds < mod.cost)
                                    ]
                                    [ text "AutoPasteis"
                                    ]
                                , text (" " ++ (toString mod.level))
                                ]
                            , div
                                []
                                [ span [] [ text ("Cost: $ " ++ (formatFloat usLocale mod.cost)) ]
                                ]
                            ]
                        ]
                )
            |> Maybe.withDefault (div [] [ text "" ])


megaPasteisView : Model -> Html Main.Msg.Msg
megaPasteisView model =
    let
        businessModule =
            model.businessModule

        manufacturingModule =
            model.manufacturingModule
    in
        manufacturingModule.megaPasteisModule
            |> Maybe.map
                (\mod ->
                    div
                        [ style [ ( "margin-top", "10px" ) ] ]
                        [ div []
                            [ div []
                                [ button
                                    [ onClick BuyMegaPasteis
                                    , disabled (businessModule.funds < mod.cost)
                                    ]
                                    [ text "MegaPasteis"
                                    ]
                                , text (" " ++ (toString mod.level))
                                ]
                            , div
                                []
                                [ span [] [ text ("Cost: $ " ++ (formatFloat usLocale mod.cost)) ]
                                ]
                            ]
                        ]
                )
            |> Maybe.withDefault (div [] [ text "" ])


updateModel : ManufacturingModule -> BusinessModule -> ManufacturingModule
updateModel model business =
    { model
        | pasteisModule = (tryMakePasteisModule model business.funds)
        , megaPasteisModule = (tryMakeMegaPasteisModule model)
    }


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
            case model.pasteisModule of
                Nothing ->
                    Nothing

                Just pasteisModule ->
                    case pasteisModule.level >= 75 of
                        False ->
                            Nothing

                        True ->
                            Just
                                { cost = 1000
                                , boost = 1
                                , level = 0
                                , factor = 500
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


update : Manufacturing.Msg -> ManufacturingModule -> ( ManufacturingModule, Cmd Main.Msg.Msg )
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


createPastel : ManufacturingModule -> Int -> ( ManufacturingModule, Cmd Main.Msg.Msg )
createPastel manufacturingModule availableDough =
    if availableDough < 1 then
        ( manufacturingModule, Cmd.none )
    else
        ( { manufacturingModule | dough = manufacturingModule.dough - 1 }
        , Task.perform NewPasteisBaked (Task.succeed 1)
        )


makePasteis : Model -> Model
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
                { model
                    | manufacturingModule = newManufacturingModule
                }

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
                { model
                    | pasteis = model.pasteis + fullPasteis
                    , businessModule = Business.addItems model.businessModule fullPasteis
                    , manufacturingModule = newManufacturingModule
                }


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


buyDough : ManufacturingModule -> Float -> ( ManufacturingModule, Cmd Main.Msg.Msg )
buyDough model funds =
    let
        cost =
            toFloat model.doughCost
    in
        if (funds < cost) then
            ( model, Cmd.none )
        else
            ( { model
                | doughBasePrice = model.doughBasePrice + 0.05
                , dough = model.dough + model.doughSupply
              }
            , Task.perform DoughtBought (Task.succeed cost)
            )
