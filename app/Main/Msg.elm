module Main.Msg exposing (..)

import Business.Msg as Business exposing (Msg)
import Manufacturing.Msg as Manufacturing exposing (Msg)
import Projects.Msg as Projects exposing (Msg)
import Projects.Model exposing (Project)
import Cheats.Model exposing (CheatModel)
import Time exposing (Time)


type Msg
    = BusinessMessage Business.Msg
    | ManufacturingMessage Manufacturing.Msg
    | ProjectsMessage Projects.Msg
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
    | ApplyCheat CheatModel
