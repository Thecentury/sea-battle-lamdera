module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Maybe
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
            "·"

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


shipIsHit : Cell -> Bool
shipIsHit cell =
    case cell of
        Empty ->
            False

        EmptyHit ->
            False

        Ship _ _ ->
            True


fieldSize : Int
fieldSize =
    10


maxFieldCoord : Int
maxFieldCoord =
    fieldSize - 1


type alias Coord =
    { x : Int, y : Int }


coordAnd9Neighbours : Coord -> List Coord
coordAnd9Neighbours coord =
    let
        x =
            coord.x

        y =
            coord.y
    in
    [ { x = x - 1, y = y - 1 }
    , { x = x + 0, y = y - 1 }
    , { x = x + 1, y = y - 1 }
    , { x = x - 1, y = y + 0 }
    , { x = x + 0, y = y + 0 }
    , { x = x + 1, y = y + 0 }
    , { x = x - 1, y = y + 1 }
    , { x = x + 0, y = y + 1 }
    , { x = x + 1, y = y + 1 }
    ]


type alias CoordTuple =
    ( Int, Int )


coordAsTuple : Coord -> CoordTuple
coordAsTuple coord =
    ( coord.x, coord.y )


type alias Field =
    Array (Array Cell)


emptyField : Field
emptyField =
    Array.initialize fieldSize (always (Array.initialize fieldSize (always Empty)))


fieldWithCoordinates : Field -> Array (Array ( Cell, Coord ))
fieldWithCoordinates field =
    Array.indexedMap
        (\y row -> Array.indexedMap (\x cell -> ( cell, { x = x, y = y } )) row)
        field



-- todo rename?


whereCoordinates : (Cell -> Bool) -> Field -> List Coord
whereCoordinates predicate field =
    field
        |> fieldWithCoordinates
        |> Array.toList
        |> List.concatMap Array.toList
        |> List.filter (predicate << Tuple.first)
        |> List.map Tuple.second


flattenField : Field -> List Cell
flattenField field =
    field
        |> Array.toList
        |> List.concatMap Array.toList


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
        |> Maybe.andThen (Array.get coord.x)


getCellOrEmpty : Field -> Coord -> Cell
getCellOrEmpty field coord =
    getCell field coord
        |> Maybe.withDefault Empty


maybeSetCell : Int -> a -> Array a -> Maybe (Array a)
maybeSetCell index value array =
    if 0 <= index && index < Array.length array then
        Just (Array.set index value array)

    else
        Nothing


setCell : Coord -> Cell -> Field -> Maybe Field
setCell coord cell field =
    Array.get coord.y field
        |> Maybe.andThen (\row -> maybeSetCell coord.x cell row)
        |> Maybe.andThen (\row -> maybeSetCell coord.y row field)


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


allShipsAreKilled : Field -> Bool
allShipsAreKilled field =
    field
        |> flattenField
        |> List.filterMap extractShip
        |> List.all (\( _, health ) -> health == Dead)


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
    [ List.range 0 (center.x - 1) |> List.map (\x -> { x = x, y = center.y }) |> List.reverse
    , List.range (center.x + 1) maxFieldCoord |> List.map (\x -> { x = x, y = center.y })
    , List.range 0 (center.y - 1) |> List.map (\y -> { x = center.x, y = y }) |> List.reverse
    , List.range (center.y + 1) maxFieldCoord |> List.map (\y -> { x = center.x, y = y })
    ]
        |> List.filter (not << List.isEmpty)


detectKilledShips : Coord -> Field -> Field
detectKilledShips hitCoord field =
    let
        otherShipCells =
            enumerateToSides hitCoord
                |> List.concatMap (\side -> side |> List.map (getCellOrEmpty field) |> List.takeWhile cellIsShip)

        allShipCells =
            getCellOrEmpty field hitCoord :: otherShipCells

        shipData =
            allShipCells |> List.filterMap extractShip

        shipIsKilled =
            shipData |> List.all (\( _, health ) -> health == Wounded)

        shipCoordsWithoutHit =
            enumerateToSides hitCoord
                |> List.concatMap
                    (\side ->
                        side
                            |> List.filterMap (\coord -> getCell field coord |> Maybe.map (Tuple.pair coord))
                            |> List.takeWhile (Tuple.second >> cellIsShip)
                            |> List.map Tuple.first
                    )
    in
    if shipIsKilled then
        let
            shipCoords =
                hitCoord :: shipCoordsWithoutHit
        in
        shipCoords
            |> List.foldl (mapCell (\cell -> killShip cell |> Maybe.withDefault cell)) field

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


fieldViewForOpponent : PlayerField -> PlayerField
fieldViewForOpponent playerField =
    { playerField | field = mapField cellViewForOpponent playerField.field }


getOpponentField : Player -> GameInProgressData -> Field
getOpponentField player data =
    case player of
        Player1 ->
            data.player2.field

        Player2 ->
            data.player1.field


updateOpponentField : Player -> Field -> GameInProgressData -> GameInProgressData
updateOpponentField player playerField data =
    case player of
        Player1 ->
            { data | player2 = withPlayerField playerField data.player2 }

        Player2 ->
            { data | player1 = withPlayerField playerField data.player1 }


addPlayerShot : Player -> Coord -> GameInProgressData -> GameInProgressData
addPlayerShot player coord data =
    case player of
        Player1 ->
            { data | player1 = data.player1 |> addShot coord }

        Player2 ->
            { data | player2 = data.player2 |> addShot coord }


type alias FrontendInitial =
    { key : Nav.Key }


type alias FrontendWaitingForAnotherPlayer =
    { key : Nav.Key
    , gameId : GameId
    }


type alias FrontendReady =
    { key : Nav.Key
    , gameId : GameId
    , next : Next FrontendPlayer
    , ownField : Field
    , opponentField : Field
    , lastOwnShot : Maybe Coord
    , lastOpponentShot : Maybe Coord
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


opponent : Player -> Player
opponent player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1


type alias PlayerField =
    { sessionId : SessionId

    -- todo store a set of client ids
    , clientId : ClientId
    , field : Field
    , shots : List Coord
    }


addShot : Coord -> PlayerField -> PlayerField
addShot coord playerField =
    { playerField | shots = List.append playerField.shots [ coord ] }


playerFieldWithClientId : ClientId -> PlayerField -> PlayerField
playerFieldWithClientId clientId playerField =
    { playerField | clientId = clientId }


withPlayerField : Field -> PlayerField -> PlayerField
withPlayerField field playerField =
    { playerField | field = field }


type alias GameInProgressData =
    { player1 : PlayerField
    , player2 : PlayerField
    , next : Next Player
    }


withNext : Next Player -> GameInProgressData -> GameInProgressData
withNext next data =
    { data | next = next }


withClientId : ClientId -> Player -> GameInProgressData -> GameInProgressData
withClientId clientId player data =
    case player of
        Player1 ->
            { data | player1 = playerFieldWithClientId clientId data.player1 }

        Player2 ->
            { data | player2 = playerFieldWithClientId clientId data.player2 }



-- todo store game started time, last access times


type BackendGameState
    = Player1Connected PlayerField
    | BothPlayersConnected GameInProgressData


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
    = Player1FieldGenerated GameId SessionId ClientId Field
    | Player2FieldGenerated GameId SessionId ClientId PlayerField Field


type FrontendPlayer
    = Me
    | Opponent


type Next p
    = Turn p
    | Winner p


type alias FrontendGameState =
    { ownField : Field
    , opponentField : Field
    , next : Next FrontendPlayer
    , lastOwnShot : Maybe Coord
    , lastOpponentShot : Maybe Coord
    }


type ToFrontend
    = GameCreated GameId
      -- todo extract into some "GameConnectError"
    | GameIsUnknown GameId
    | ClickedCellRejected Coord
    | ToFrontendError String
    | UpdateGameState FrontendGameState
