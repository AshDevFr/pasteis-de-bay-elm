module Main.Msg exposing (..)

import Business.Msg exposing (Msg)
import Manufacturing.Msg exposing (Msg)
import Projects.Msg exposing (Msg)
import Projects.Model exposing (Project)
import Time exposing (Time)


type Msg
    = BusinessMessage Business.Msg.Msg
    | ManufacturingMessage Manufacturing.Msg.Msg
    | ProjectsMessage Projects.Msg.Msg
    | Tick Time
    | UpdateModel
    | BuyPasteis
    | BuyMegaPasteis
    | AddProcessor
    | AddMemory
    | Reset
    | NewPasteisBaked Int
    | DoughtBought Float
    | ActivateProject Project
