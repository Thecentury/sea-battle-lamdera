module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Maybe exposing (andThen)
import Maybe.Extra as Maybe
import Url exposing (Url)


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


mapCell : (Cell -> Cell) -> Coord -> Field -> Field
mapCell f coord field =
    getCell field coord
        |> Maybe.andThen (\cell -> setCell coord (f cell) field)
        |> Maybe.withDefault field


getCell : Field -> Coord -> Maybe Cell
getCell field coord =
    Array.get coord.y field
        |> andThen (Array.get coord.x)


setCell : Coord -> Cell -> Field -> Maybe Field
setCell coord cell field =
    Array.get coord.y field
        |> Maybe.map (\row -> Array.set coord.x cell row)
        |> Maybe.map (\row -> Array.set coord.y row field)


hitCell : Cell -> Maybe Cell
hitCell cell =
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


cellIsShip : Cell -> Bool
cellIsShip cell =
    case cell of
        Empty ->
            False

        EmptyHit ->
            False

        Ship _ _ ->
            True


extractShip : Cell -> Maybe ( ShipSize, ShipHealth )
extractShip cell =
    case cell of
        Empty ->
            Nothing

        EmptyHit ->
            Nothing

        Ship size health ->
            Just ( size, health )


killShip : Cell -> Maybe Cell
killShip cell =
    case cell of
        Empty ->
            Nothing

        EmptyHit ->
            Nothing

        Ship size health ->
            case health of
                Alive ->
                    Nothing

                Wounded ->
                    Just <| Ship size Dead

                Dead ->
                    Nothing


enumerateToSides : Coord -> List (List Coord)
enumerateToSides center =
    [ List.range 0 (center.x - 1) |> List.map (\x -> { x = x, y = center.y })
    , List.range (center.x + 1) (fieldSize - 1) |> List.map (\x -> { x = x, y = center.y })
    , List.range 0 (center.y - 1) |> List.map (\y -> { x = center.x, y = y })
    , List.range (center.y + 1) (fieldSize - 1) |> List.map (\y -> { x = center.x, y = y })
    ]
        |> List.filter (not << List.isEmpty)


detectKilledShips : Coord -> Field -> Field
detectKilledShips hitCoord field =
    let
        otherShipCells =
            enumerateToSides hitCoord
                |> List.concatMap (\side -> side |> List.filterMap (getCell field) |> List.takeWhile cellIsShip)

        allShipCells =
            Maybe.toList (getCell field hitCoord)
                |> List.append otherShipCells

        shipData =
            allShipCells |> List.filterMap extractShip

        shipIsKilled =
            List.all (\( _, health ) -> health == Wounded) shipData

        shipCoordsWithoutHit =
            enumerateToSides hitCoord
                |> List.concatMap
                    (\side ->
                        side
                            |> List.filterMap (\coord -> getCell field coord |> Maybe.map (Tuple.pair coord))
                            |> List.takeWhile (Tuple.second >> cellIsShip)
                            |> List.map Tuple.first
                    )

        shipCoords =
            hitCoord :: shipCoordsWithoutHit
    in
    if shipIsKilled then
        shipCoords
            |> List.foldl (\coord f -> mapCell (\cell -> killShip cell |> Maybe.withDefault cell) coord f) field

    else
        field


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


frontendGameId : FrontendModel -> Maybe GameId
frontendGameId model =
    case model of
        Initial _ ->
            Nothing

        WaitingForAnotherPlayer data ->
            Just data.gameId

        Playing data ->
            Just data.gameId


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

    -- todo store a set of client ids
    , clientId : ClientId
    , playerField : Field

    -- todo do not store enemy field but compute it?
    , enemyField : Field
    }


playerFieldWithClientId : ClientId -> PlayerField -> PlayerField
playerFieldWithClientId clientId playerField =
    { playerField | clientId = clientId }


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


withClientId : ClientId -> Player -> BothPlayersConnectedData -> BothPlayersConnectedData
withClientId clientId player data =
    case player of
        Player1 ->
            { data | player1 = playerFieldWithClientId clientId data.player1 }

        Player2 ->
            { data | player2 = playerFieldWithClientId clientId data.player2 }



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
    = GameCreated GameId
      -- todo extract into some "GameConnectError"
    | GameIsUnknown GameId
    | ClickedCellRejected Coord
    | ToFrontendError String
    | UpdateGameState UpdatedGameState
