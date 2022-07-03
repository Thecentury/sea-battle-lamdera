module Backend exposing (..)

import Dict
import FieldGeneration
import Lamdera exposing (ClientId, SessionId)
import Random
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
    ( { latestGameId = 0, games = Dict.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Player1FieldGenerated gameId sessionId clientId field ->
            let
                game =
                    Player1Connected
                        { sessionId = sessionId
                        , clientId = clientId
                        , field = field
                        }

                nextModel =
                    updateGame gameId game { model | latestGameId = gameId }
            in
            ( nextModel, Lamdera.sendToFrontend clientId (GameCreated gameId) )

        Player2FieldGenerated gameId sessionId clientId player1Field field ->
            let
                player2Field =
                    { sessionId = sessionId
                    , clientId = clientId
                    , field = field
                    }

                game =
                    { player1 = player1Field
                    , player2 = player2Field
                    , next = Turn Player1
                    }

                model2 =
                    updateGame gameId (BothPlayersConnected game) model
            in
            ( model2
            , Cmd.batch
                [ Lamdera.sendToFrontend player1Field.clientId (UpdateGameState (createFrontendUpdateForPlayer Player1 game))
                , Lamdera.sendToFrontend player2Field.clientId (UpdateGameState (createFrontendUpdateForPlayer Player2 game))
                ]
            )


updateGame : GameId -> BackendGameState -> BackendModel -> BackendModel
updateGame gameId state model =
    { model | games = Dict.insert gameId state model.games }


nextToFrontend : Player -> Next Player -> Next FrontendPlayer
nextToFrontend player next =
    case next of
        Turn nextPlayer ->
            if player == nextPlayer then
                Turn Me

            else
                Turn Opponent

        Winner winner ->
            if player == winner then
                Winner Me

            else
                Winner Opponent


createFrontendGameUpdate : Player -> Next Player -> Field -> Field -> FrontendGameState
createFrontendGameUpdate me next myField opponentField =
    { ownField = myField
    , opponentField = opponentField
    , next = nextToFrontend me next
    }


createFrontendUpdateForPlayer : Player -> GameInProgressData -> FrontendGameState
createFrontendUpdateForPlayer player data =
    case player of
        Player1 ->
            createFrontendGameUpdate Player1 data.next data.player1.field (fieldViewForOpponent data.player2.field)

        Player2 ->
            createFrontendGameUpdate Player2 data.next data.player2.field (fieldViewForOpponent data.player1.field)


playerFromSessionId : SessionId -> GameInProgressData -> Maybe Player
playerFromSessionId sessionId data =
    if sessionId == data.player1.sessionId then
        Just Player1

    else if sessionId == data.player2.sessionId then
        Just Player2

    else
        Nothing


handleCellClicked : ClientId -> SessionId -> GameInProgressData -> Coord -> ( GameInProgressData, List (Cmd BackendMsg) )
handleCellClicked clientId sessionId data coord =
    case playerFromSessionId sessionId data of
        Just player ->
            case data.next of
                Turn currentTurn ->
                    if player == currentTurn then
                        let
                            opponentField =
                                getOpponentField currentTurn data

                            mUpdatedOpponentField =
                                getCell opponentField coord
                                    |> Maybe.andThen hitCell
                                    |> Maybe.andThen (\cell -> setCell coord cell opponentField)
                                    |> Maybe.map (detectKilledShips coord)
                        in
                        case mUpdatedOpponentField of
                            Nothing ->
                                ( data, [ Lamdera.sendToFrontend clientId (ClickedCellRejected coord) ] )

                            Just updatedOpponentField ->
                                let
                                    playerWon =
                                        allShipsAreKilled updatedOpponentField
                                in
                                if playerWon then
                                    let
                                        updatedData =
                                            updateOpponentField currentTurn updatedOpponentField data

                                        commands =
                                            [ Lamdera.sendToFrontend data.player1.clientId (UpdateGameState (createFrontendUpdateForPlayer Player1 updatedData))
                                            , Lamdera.sendToFrontend data.player2.clientId (UpdateGameState (createFrontendUpdateForPlayer Player2 updatedData))
                                            ]
                                    in
                                    ( { player1 = updatedData.player1
                                      , player2 = updatedData.player2
                                      , next = Winner player
                                      }
                                    , commands
                                    )

                                else
                                    let
                                        updatedCell =
                                            getCellOrEmpty updatedOpponentField coord

                                        nextPlayer =
                                            if shipIsHit updatedCell then
                                                currentTurn

                                            else
                                                opponent currentTurn

                                        updatedData =
                                            updateOpponentField currentTurn updatedOpponentField data
                                                |> withTurn nextPlayer

                                        commands =
                                            [ Lamdera.sendToFrontend data.player1.clientId (UpdateGameState (createFrontendUpdateForPlayer Player1 updatedData))
                                            , Lamdera.sendToFrontend data.player2.clientId (UpdateGameState (createFrontendUpdateForPlayer Player2 updatedData))
                                            ]
                                    in
                                    ( updatedData, commands )

                    else
                        ( data, [ Lamdera.sendToFrontend clientId (ToFrontendError "It's not your turn") ] )

                Winner _ ->
                    ( data, [ Lamdera.sendToFrontend clientId (ToFrontendError "Game is over") ] )

        Nothing ->
            ( data, [ Lamdera.sendToFrontend clientId (ToFrontendError "You don't belong to this game") ] )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case Debug.log ("Backend message, session " ++ sessionId ++ ", client " ++ clientId) msg of
        ConnectToGame gameId ->
            case Dict.get gameId model.games of
                Just game ->
                    case game of
                        Player1Connected player1Field ->
                            if player1Field.sessionId == sessionId then
                                -- Just update clientId
                                ( updateGame gameId (Player1Connected { player1Field | clientId = clientId }) model, Cmd.none )

                            else
                                ( model, Random.generate (Player2FieldGenerated gameId sessionId clientId player1Field) FieldGeneration.fieldGenerator )

                        BothPlayersConnected data ->
                            let
                                maybePlayer =
                                    playerFromSessionId sessionId data

                                commands =
                                    maybePlayer
                                        |> Maybe.map (\player -> UpdateGameState (createFrontendUpdateForPlayer player data))
                                        |> Maybe.withDefault (ToFrontendError <| "Unknown client id " ++ clientId ++ ", session id " ++ sessionId)
                                        |> List.singleton
                                        |> List.map (Lamdera.sendToFrontend clientId)

                                model2 =
                                    maybePlayer
                                        |> Maybe.map (\player -> updateGame gameId (BothPlayersConnected (withClientId clientId player data)) model)
                                        |> Maybe.withDefault model
                            in
                            ( model2, Cmd.batch commands )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId (GameIsUnknown gameId) )

        CellClicked gameId coord ->
            case Dict.get gameId model.games of
                Just game ->
                    case game of
                        Player1Connected _ ->
                            ( model, Lamdera.sendToFrontend clientId (ToFrontendError "Game is in Player1Connected state (BothPlayersConnected expected)") )

                        BothPlayersConnected data ->
                            let
                                ( updatedGame, commands ) =
                                    handleCellClicked clientId sessionId data coord
                            in
                            ( updateGame gameId (BothPlayersConnected updatedGame) model, Cmd.batch commands )

                Nothing ->
                    ( Debug.log "Game not found" model, Lamdera.sendToFrontend clientId (GameIsUnknown gameId) )

        CreateNewGame ->
            let
                gameId =
                    model.latestGameId + 1

                nextModel =
                    { model | latestGameId = gameId }
            in
            ( nextModel, Random.generate (Player1FieldGenerated gameId sessionId clientId) FieldGeneration.fieldGenerator )
