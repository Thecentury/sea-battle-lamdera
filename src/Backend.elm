module Backend exposing (..)

import Array
import Dict
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel



--noinspection ElmUnusedSymbol


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { latestGameId = 0, games = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateGame : GameId -> BackendGameState -> BackendModel -> BackendModel
updateGame gameId state model =
    { model | games = Dict.insert gameId state model.games }


fieldFromString : String -> Field
fieldFromString str =
    let
        parseCell : Char -> Cell
        parseCell c =
            case c of
                '1' ->
                    Ship Size1 Alive

                '2' ->
                    Ship Size2 Alive

                '3' ->
                    Ship Size3 Alive

                '4' ->
                    Ship Size4 Alive

                _ ->
                    Empty
    in
    str
        |> String.lines
        |> List.filter (\l -> String.length l > 0)
        |> List.map (String.trim >> String.toList >> List.map parseCell >> Array.fromList)
        |> Array.fromList


player1InitialField : Field
player1InitialField =
    fieldFromString
        """
        0100000010
        0004444000
        0000000030
        0100000030
        0000000030
        2200000000
        0000000220
        0000000000
        0033302010
        0000002000
        """


player2InitialField : Field
player2InitialField =
    fieldFromString
        """
        0002000000
        0002000001
        0000002200
        4000000001
        4000000200
        4001000200
        4000000000
        0033300001
        0000000000
        0000033300
        """


emptyField : Field
emptyField =
    Array.initialize fieldSize (always (Array.initialize fieldSize (always Empty)))


createFrontendGameUpdate : Player -> Player -> Field -> Field -> UpdatedGameState
createFrontendGameUpdate me turn myField enemyField =
    { ownField = myField
    , enemyField = enemyField
    , me = me
    , turn = turn
    }


createFrontendUpdateForPlayer : Player -> BothPlayersConnectedData -> UpdatedGameState
createFrontendUpdateForPlayer player data =
    case player of
        Player1 ->
            createFrontendGameUpdate Player1 data.turn data.player1.playerField data.player1.enemyField

        Player2 ->
            createFrontendGameUpdate Player2 data.turn data.player2.playerField data.player2.enemyField


playerFromSessionId : SessionId -> BothPlayersConnectedData -> Maybe Player
playerFromSessionId sessionId data =
    if sessionId == data.player1.sessionId then
        Just Player1

    else if sessionId == data.player2.sessionId then
        Just Player2

    else
        Nothing


handleCellClicked : BothPlayersConnectedData -> Coord -> ( BothPlayersConnectedData, List (Cmd BackendMsg) )
handleCellClicked data coord =
    -- todo validate clientId to ensure that the message is sent from a proper player
    -- todo proper handling of a current player
    let
        field =
            opponentField data.turn data

        updated =
            getCell coord field
                |> Maybe.andThen hit
                |> Maybe.andThen (\cell -> setCell coord cell field)
    in
    -- todo handle if killed
    case updated of
        Nothing ->
            ( data, [] )

        Just updatedField ->
            let
                updatedData =
                    withOpponentField data.turn updatedField data
                        |> withTurn (nextPlayer data.turn)

                commands =
                    [ Lamdera.sendToFrontend data.player1.clientId (UpdateGameState (createFrontendUpdateForPlayer Player1 updatedData))
                    , Lamdera.sendToFrontend data.player2.clientId (UpdateGameState (createFrontendUpdateForPlayer Player2 updatedData))
                    ]
            in
            ( updatedData, commands )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ConnectToGame gameId ->
            case Dict.get gameId model.games of
                Just game ->
                    case game of
                        Player1Connected player1Field ->
                            if player1Field.sessionId == sessionId then
                                ( model, Cmd.none )

                            else
                                let
                                    player2Field =
                                        { sessionId = sessionId
                                        , clientId = clientId
                                        , playerField = player2InitialField
                                        , enemyField = emptyField
                                        }

                                    game2 =
                                        { player1 = player1Field
                                        , player2 = player2Field
                                        , turn = Player1
                                        }

                                    model2 =
                                        updateGame gameId (BothPlayersConnected game2) model
                                in
                                ( model2
                                , Cmd.batch
                                    [ Lamdera.sendToFrontend player1Field.clientId (UpdateGameState (createFrontendUpdateForPlayer Player1 game2))
                                    , Lamdera.sendToFrontend player2Field.clientId (UpdateGameState (createFrontendUpdateForPlayer Player2 game2))
                                    ]
                                )

                        BothPlayersConnected data ->
                            let
                                maybePlayer =
                                    playerFromSessionId clientId data

                                commands =
                                    maybePlayer
                                        |> Maybe.map (\player -> UpdateGameState (createFrontendUpdateForPlayer player data))
                                        |> Maybe.withDefault (ToFrontendError <| "Unknown client id " ++ clientId ++ ", session id " ++ sessionId)
                                        |> List.singleton
                                        |> List.map (Lamdera.sendToFrontend clientId)
                            in
                            ( model, Cmd.batch commands )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId (GameIsUnknown gameId) )

        CellClicked gameId coord ->
            case Dict.get gameId model.games of
                Just game ->
                    case game of
                        Player1Connected _ ->
                            -- todo reply with error
                            ( model, Cmd.none )

                        BothPlayersConnected data ->
                            let
                                ( data2, commands ) =
                                    handleCellClicked data coord
                            in
                            ( updateGame gameId (BothPlayersConnected data2) model, Cmd.batch commands )

                Nothing ->
                    -- todo reply with error
                    ( model, Cmd.none )

        CreateNewGame ->
            let
                gameId =
                    model.latestGameId + 1

                game =
                    Player1Connected
                        { sessionId = sessionId
                        , clientId = clientId
                        , playerField = player1InitialField
                        , enemyField = emptyField
                        }

                nextModel =
                    updateGame gameId game { model | latestGameId = gameId }
            in
            ( nextModel, Lamdera.sendToFrontend clientId (GameCreated gameId) )
