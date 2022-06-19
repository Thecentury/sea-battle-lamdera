module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
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


type alias PlayerField =
    { playerId : ClientId
    , field : Field
    }


type alias BothPlayersConnectedData =
    { player1 : PlayerField
    , player2 : PlayerField
    , turn : Player
    }



-- todo store game started time, last access times


type BackendGameState
    = Player1Connected PlayerField
    | BothPlayersConnected BothPlayersConnectedData


type alias GameId =
    Int


type alias BackendModel =
    { games : Dict GameId BackendGameState
    , latestGameId : GameId
    }


type Route
    = Player1Route
    | Player2Route


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | UserClickedCell Coord


type ToBackend
    = CreateNewGame
    | ConnectToGame GameId
    | CellClicked GameId Coord


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
      -- todo extract into some "GameConnectError"
    | GameIsUnknown
