module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Maybe exposing (andThen)
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
      -- todo this case can be only for an opponent
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


mapField : (Cell -> Cell) -> Field -> Field
mapField f field =
    Array.map (Array.map f) field


getCell : Coord -> Field -> Maybe Cell
getCell coord field =
    Array.get coord.y field
        |> andThen (Array.get coord.x)


setCell : Coord -> Cell -> Field -> Maybe Field
setCell coord cell field =
    Array.get coord.y field
        |> Maybe.map (\row -> Array.set coord.x cell row)
        |> Maybe.map (\row -> Array.set coord.y row field)


hit : Cell -> Maybe Cell
hit cell =
    case cell of
        Empty ->
            Just EmptyHit

        EmptyHit ->
            Nothing

        Ship size health ->
            case health of
                Alive ->
                    Just <| Ship size Wounded

                Wounded ->
                    Nothing

                Dead ->
                    Nothing


cellViewForOpponent : Cell -> Cell
cellViewForOpponent cell =
    case cell of
        Empty ->
            Empty

        EmptyHit ->
            EmptyHit

        Ship size health ->
            case health of
                Alive ->
                    Empty

                Wounded ->
                    Ship UnknownSize Wounded

                Dead ->
                    Ship size Dead


fieldViewForOpponent : Field -> Field
fieldViewForOpponent field =
    mapField cellViewForOpponent field


opponentField : Player -> BothPlayersConnectedData -> Field
opponentField player data =
    case player of
        Player1 ->
            data.player2.playerField

        Player2 ->
            data.player1.playerField


withOpponentField : Player -> Field -> BothPlayersConnectedData -> BothPlayersConnectedData
withOpponentField player field data =
    case player of
        Player1 ->
            { data
                | player2 = withPlayerField field data.player2
                , player1 = withEnemyField (fieldViewForOpponent field) data.player1
            }

        Player2 ->
            { data
                | player1 = withPlayerField field data.player1
                , player2 = withEnemyField (fieldViewForOpponent field) data.player2
            }


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


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1


type alias PlayerField =
    { sessionId : SessionId
    , clientId : ClientId
    , playerField : Field

    -- todo do not store enemy field but compute it?
    , enemyField : Field
    }


withPlayerField : Field -> PlayerField -> PlayerField
withPlayerField field playerField =
    { playerField | playerField = field }


withEnemyField : Field -> PlayerField -> PlayerField
withEnemyField field playerField =
    { playerField | enemyField = field }


type alias BothPlayersConnectedData =
    { player1 : PlayerField
    , player2 : PlayerField
    , turn : Player
    }


withTurn : Player -> BothPlayersConnectedData -> BothPlayersConnectedData
withTurn turn data =
    { data | turn = turn }



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
