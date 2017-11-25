module Views.Main exposing (view)

import FormatNumber exposing (formatFloat, formatInt, usLocale)
import Html exposing (..)
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, Device(..), Align(..), align)
import Models exposing (..)
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Business as Business
import Manufacturing exposing (..)
import Manufacturing.Msg exposing (..)
import Computing as Computing
import Material.Scheme


view : Model -> Html Models.Msg
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
                    , Options.onClick (ManufacturingMessage (Manufacturing.Msg.BakePastel 1))
                    , Options.disabled (model.manufacturingModule.dough < 1)
                    ]
                    [ text "Make a Pastel" ]
                , Button.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Button.icon
                    , Button.colored
                    , Button.ripple
                    , Options.onClick Reset
                    , css "margin-left" "10px"
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
