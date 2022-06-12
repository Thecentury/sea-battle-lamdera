module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId)
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
    , route : Maybe Route
    , ownField : Field
    , enemyField : Field
    }


type Player
    = Player1
    | Player2


type alias BothPlayersConnectedData =
    { player1 : ClientId
    , field1 : Field
    , player2 : ClientId
    , field2 : Field
    }


type BackendModelState
    = NoPlayersState
    | Player1Connected ClientId
    | BothPlayersConnected BothPlayersConnectedData


type alias BackendModel =
    BackendModelState


type Route
    = Player1Route
    | Player2Route


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | UserClickedCell Coord


type ToBackend
    = CellClicked Coord


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
