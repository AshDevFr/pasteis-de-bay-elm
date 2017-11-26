module Main.Model exposing (..)

import Business.Model exposing (BusinessModule)
import Manufacturing.Model exposing (ManufacturingModule)
import Computing.Model exposing (ComputingModule)
import Projects.Module.ProjectsModule exposing (ProjectsModule, ProjectsModuleSave)
import Time exposing (Time)


type alias SaveModel =
    { pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    , computingModule : Maybe ComputingModule
    , projectsModule : Maybe ProjectsModuleSave
    }


type alias Model =
    { lastTick : Maybe Time
    , pasteis : Int
    , businessModule : BusinessModule
    , manufacturingModule : ManufacturingModule
    , computingModule : Maybe ComputingModule
    , projectsModule : Maybe ProjectsModule
    }
