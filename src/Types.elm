module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type ShipState
    = Alive
    | Wounded
    | Dead


type ShipKind
    = Size1
    | Size2
    | Size3
    | Size4
    | Unknown


type Cell
    = Empty
    | EmptyHit
    | Ship ShipKind ShipState


cellToString : Cell -> String
cellToString cell =
    case cell of
        Empty ->
            " "

        EmptyHit ->
            "*"

        Ship kind state ->
            "s"


fieldSize : Int
fieldSize =
    10


type alias Coord =
    { x : Int, y : Int }


type alias Field =
    Array (Array Cell)


type alias FrontendModel =
    { gameId : String
    , ownField : Field
    , enemyField : Field
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | CellClicked Coord


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
