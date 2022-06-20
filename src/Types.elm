module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Url exposing (Url)


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just v ->
            [ v ]

        Nothing ->
            []



-------------------------------------------------------------------------------


type ShipHealth
    = Alive
    | Wounded
    | Dead


type ShipSize
    = Size1
    | Size2
    | Size3
    | Size4
    | UnknownSize


type Cell
    = Empty
    | EmptyHit
    | Ship ShipSize ShipHealth


cellToString : Cell -> String
cellToString cell =
    case cell of
        Empty ->
            " "

        EmptyHit ->
            "*"

        Ship Size1 _ ->
            "1"

        Ship Size2 _ ->
            "2"

        Ship Size3 _ ->
            "3"

        Ship Size4 _ ->
            "4"

        Ship UnknownSize _ ->
            "?"


cellToBackground : Cell -> Maybe String
cellToBackground cell =
    case cell of
        Empty ->
            Nothing

        EmptyHit ->
            Nothing

        Ship _ Alive ->
            Nothing

        Ship _ Wounded ->
            Just "#FFB700FF"

        Ship _ Dead ->
            Just "#ff4e4e"


fieldSize : Int
fieldSize =
    10


type alias Coord =
    { x : Int, y : Int }


type alias Field =
    Array (Array Cell)


type alias FrontendInitial =
    { key : Nav.Key }


type alias FrontendWaitingForAnotherPlayer =
    { key : Nav.Key
    , gameId : GameId
    }


type alias FrontendReady =
    { key : Nav.Key
    , gameId : GameId
    , currentTurn : Player
    , me : Player
    , ownField : Field
    , enemyField : Field
    }


type FrontendModel
    = Initial FrontendInitial
    | WaitingForAnotherPlayer FrontendWaitingForAnotherPlayer
    | Playing FrontendReady


type Player
    = Player1
    | Player2


type alias PlayerField =
    { playerId : ClientId
    , playerField : Field
    , enemyField : Field
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
    = Root
    | GameRoot GameId


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | CreateNewGameClicked
    | UserClickedCell Coord


type ToBackend
    = CreateNewGame
    | ConnectToGame GameId
    | CellClicked GameId Coord


type BackendMsg
    = NoOpBackendMsg


type alias UpdatedGameState =
    { ownField : Field
    , enemyField : Field
    , me : Player
    , turn : Player
    }


type ToFrontend
    = NoOpToFrontend
    | GameCreated GameId
      -- todo extract into some "GameConnectError"
    | GameIsUnknown GameId
    | ToFrontendError String
    | UpdateGameState UpdatedGameState
