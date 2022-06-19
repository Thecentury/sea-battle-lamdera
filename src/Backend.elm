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


playerFromClientId : ClientId -> BothPlayersConnectedData -> Maybe Player
playerFromClientId clientId data =
    if clientId == data.player1.playerId then
        Just Player1

    else if clientId == data.player2.playerId then
        Just Player2

    else
        Nothing


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        ConnectToGame gameId ->
            case Dict.get gameId model.games of
                Just game ->
                    case game of
                        Player1Connected player1Field ->
                            if player1Field.playerId == clientId then
                                -- todo if sessionId = player1, do not update the game, just respond with the player data
                                ( model, Cmd.none )

                            else
                                let
                                    player2Field =
                                        { playerId = clientId
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
                                    [ Lamdera.sendToFrontend player1Field.playerId (UpdateGameState (createFrontendUpdateForPlayer Player1 game2))
                                    , Lamdera.sendToFrontend player2Field.playerId (UpdateGameState (createFrontendUpdateForPlayer Player2 game2))
                                    ]
                                )

                        BothPlayersConnected data ->
                            let
                                maybePlayer =
                                    playerFromClientId clientId data

                                commands =
                                    maybePlayer
                                        |> Maybe.map (\player -> Lamdera.sendToFrontend clientId (UpdateGameState (createFrontendUpdateForPlayer player data)))
                                        |> maybeToList
                            in
                            ( model, Cmd.batch commands )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId (GameIsUnknown gameId) )

        CellClicked _ _ ->
            -- todo implement me
            ( model, Cmd.none )

        CreateNewGame ->
            let
                gameId =
                    model.latestGameId + 1

                game =
                    Player1Connected
                        { playerId = clientId
                        , playerField = player1InitialField
                        , enemyField = emptyField
                        }

                nextModel =
                    updateGame gameId game { model | latestGameId = gameId }
            in
            ( nextModel, Lamdera.sendToFrontend clientId (GameCreated gameId) )
